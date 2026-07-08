package de.dnpm.dip.mtb.query.impl 


import java.time.{
  Instant,
  LocalDateTime
}
import java.util.UUID.randomUUID
import scala.concurrent.{
  ExecutionContext,
  Future
}
import cats.{
  Applicative,
  Monad
}
import cats.data.{
  EitherNel,
  NonEmptyList
}
import cats.syntax.either._
import cats.syntax.traverse._
import de.dnpm.dip.util.Logging
import de.dnpm.dip.util.Completer.syntax._
import de.dnpm.dip.service.{
  Cache,
  Connector,
  ConnectionStatus
}
import de.dnpm.dip.connector.{
  FakeConnector,
  HttpConnector,
  HttpMethod
}
import de.dnpm.dip.service.query.{
  BaseQueryService,
  Querier,
  Query,
  FederatedQuery,
  PatientRecordRequest,
  LocalDB,
  PreparedQueryDB
}
import de.dnpm.dip.coding.{
  CodeSystemProvider,
  Coding
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.{
  ICD10GM,
  ICDO3
}
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.model.{
  Id,
  Site
}
import de.dnpm.dip.mtb.model.MTBPatientRecord
import de.dnpm.dip.mtb.query.api._
import play.api.libs.json.Json


class MTBQueryServiceProviderImpl extends MTBQueryServiceProvider
{

  override def getInstance: MTBQueryService =
    return MTBQueryServiceImpl.instance

}


object MTBQueryServiceImpl extends Logging
{

  import HttpMethod._


  private val federatedQueriesActive =
    sys.env.get("ACTIVE_FEDERATED_QUERY_USE_CASES")
      .map(_.split(",").map(_.trim.toUpperCase).toSet)
      .exists(_ contains "MTB")


  private val kAnonymityFactor = {

    val default = 5

    sys.env.get("K_ANONYMITY_FACTOR")
      .map(_.toInt) match { 
        case Some(k) =>
          if (k >= default) k
          else throw new IllegalArgumentException(s"Illegal k anonymity factor $k, must not be less than $default")

        case None => default
      }
  }       


  private lazy val connector =
    System.getProperty(HttpConnector.Type.property,"broker") match {
      case HttpConnector.Type(typ) =>
        val baseURI = "/api/mtb/peer2peer"
        HttpConnector(
          typ,
          { 
            case _: FederatedQuery[_,_] =>
              (POST, s"$baseURI/query", Map.empty)

            case _: FeasibilityQuery.Request =>
              (POST, s"$baseURI/feasibility-query", Map.empty)

            case PatientRecordRequest(_,querier,patient,snapshot) =>
              (
                GET, s"$baseURI/patient-record", Map(
                  "querier" -> Seq(querier.value),
                  "patient" -> Seq(patient.value)
                ) ++ snapshot.map(snp => "snapshot" -> Seq(snp.toString))
              )
          }        
        )

      case _ =>
        import scala.concurrent.ExecutionContext.Implicits._
        log.warn("Falling back to Fake Connector!")
        FakeConnector[Future]
    }

  private[impl] lazy val instance =
    new MTBQueryServiceImpl(
      MTBPreparedQueryDB.instance,      
      MTBLocalDB.instance,
      connector,
      Cache.empty(),
      Cache.empty(),
      federatedQueriesActive,
      kAnonymityFactor
    )
}


class MTBQueryServiceImpl
(
  val preparedQueryDB: PreparedQueryDB[Future,Monad[Future],MTBQueryCriteria,String],
  val db: LocalDB[Future,Monad[Future],MTBQueryCriteria,MTBPatientRecord],
  val connector: Connector[Future,Monad[Future]],
  val querySessions: Cache[Query.Id,(Query[MTBQueryCriteria],MTBResultSet)],
  val feasibilityQuerySessions: Cache[Id[FeasibilityQuery],(FeasibilityQuery,FeasibilityQuery.AggregatedResults)],
  val federatedQueriesActive: Boolean,
  val kAnonymityFactor: Int
)
extends BaseQueryService[Future,MTBConfig]
with MTBQueryService
with Completers
{

  import FeasibilityQueryOps._
  import MTBQueryCriteriaOps.Extensions
    
    
  override implicit val hgnc: CodeSystemProvider[HGNC,cats.Id,Applicative[cats.Id]] =
    HGNC.GeneSet
      .getInstance[cats.Id]
      .get

  override implicit val atc: CodeSystemProvider[ATC,cats.Id,Applicative[cats.Id]] =
    ATC.Catalogs
      .getInstance[cats.Id]
      .get

  override implicit val icd10gm: CodeSystemProvider[ICD10GM,cats.Id,Applicative[cats.Id]] =
    ICD10GM.Catalogs
      .getInstance[cats.Id]
      .get

  override implicit val icdo3: ICDO3.Catalogs[cats.Id,Applicative[cats.Id]] =
    ICDO3.Catalogs  
      .getInstance[cats.Id]
      .get


  private implicit val kmEstimator: KaplanMeierEstimator[cats.Id] =
    DefaultKaplanMeierEstimator

  private implicit val kmModule: KaplanMeierModule[cats.Id] =
    new DefaultKaplanMeierModule



  override def validate(criteria: MTBQueryCriteria): EitherNel[String,MTBQueryCriteria] =
    Either.cond(
      criteria.nonEmpty,
      criteria,
      "Query criteria must not be empty"
    )
    .toEitherNel


  override def ResultSetFrom(
    query: Query[MTBQueryCriteria],
    results: Seq[Query.Match[MTBPatientRecord,MTBQueryCriteria]]
  ) =
    new MTBResultSetImpl(query.id,query.criteria,results)


  override val survivalConfig: KaplanMeier.Config =
    kmModule.survivalConfig


  def !(
    cmd: FeasibilityQuery.Command
  )(
    implicit
    querier: Querier,
    ctx: ExecutionContext
  ): Future[Either[Query.Error,FeasibilityQuery]] = {

    cmd match {
      case FeasibilityQuery.Submit(mode,rawCriteria) =>

        validate(rawCriteria).map(_.complete) match {
          case Right(criteria) =>
            log.info(s"Processing new FeasibilityQuery by $querier: \n${Json.prettyPrint(Json.toJson(criteria))}")

            val id = Id[FeasibilityQuery](randomUUID.toString)
            
            for {
              resultsBySite <- executeFeasibilityQuery(id,mode.code.enumValue,criteria)
            
              errsOrLocalResults =
                resultsBySite
                  .values
                  .map(_.map(NonEmptyList.one).toIor.toIorNel)
                  // Note: Call to reduce is implicitly safe here, because the local site is always queried,
                  // so the returned Map is nonEmpty, but consider making explicitly safe (reduceOption...) TODO?
                  .reduce(_ combine _)
                  .toEither  
            
              outcome = errsOrLocalResults match {
                case Right(localResults) =>
                  FeasibilityQuery.AggregatedResults.of(id,localResults,kAnonymityFactor) match {
                
                    case Some(aggregatedResult) =>
                      val query = FeasibilityQuery(
                        id,
                        LocalDateTime.now,
                        querier,
                        mode,
                        criteria,
                        ConnectionStatus.from(resultsBySite),
                        sessionTimeout.toSeconds.toInt,
                        Instant.now 
                      )
            
                      feasibilityQuerySessions.put(id,(query,aggregatedResult),sessionTimeout)
            
                      query.asRight  
            
                    case None => Query.NoResults.asLeft
                  }
            
                case Left(errs) => Query.ConnectionErrors(errs).asLeft
              }
            
            } yield outcome

          case Left(errors) => Future.successful(Query.InvalidCriteria(errors).asLeft)
        }


      case FeasibilityQuery.Update(id,optMode,optRawCriteria) =>

        feasibilityQuerySessions.get(id).map(_._1) match {
          case Some(query) =>

            optRawCriteria.traverse(validate).map(_.complete) match {
              case Right(optCriteria) =>
            
                if (optMode.exists(_ != query.mode) || optCriteria.exists(_ != query.criteria)){
                
                  val mode = optMode.getOrElse(query.mode)
                  val criteria = optCriteria.getOrElse(query.criteria)

                  log.info(s"Updating FeasibilityQuery $id: \n${Json.prettyPrint(Json.toJson(criteria))}")
            
                  for {
                    resultsBySite <- executeFeasibilityQuery(id,mode.code.enumValue,criteria)
                  
                    errsOrLocalResults =
                      resultsBySite
                        .values
                        .map(_.map(NonEmptyList.one).toIor.toIorNel)
                        .reduce(_ combine _) //TODO: consider using safe operation
                        .toEither  
                  
                    outcome = errsOrLocalResults match {
                      case Right(localResults) =>
                        FeasibilityQuery.AggregatedResults.of(id,localResults,kAnonymityFactor) match {
                      
                          case Some(aggregatedResult) =>
                            val updatedQuery = query.copy(
                              mode = mode,
                              criteria = criteria,
                              peers = ConnectionStatus.from(resultsBySite),
                              lastUpdate = Instant.now 
                            )
                  
                            feasibilityQuerySessions.put(id,(updatedQuery,aggregatedResult),sessionTimeout)
                  
                            updatedQuery.asRight  
                  
                          case None => Query.NoResults.asLeft
                        }
                  
                      case Left(errs) => Query.ConnectionErrors(errs).asLeft
                    }
                  
                  } yield outcome
            
                // Unchanged
                } else Future.successful(query.asRight)

              case Left(errors) => Future.successful(Query.InvalidCriteria(errors).asLeft)
            }

            case None => Future.successful(Query.InvalidId.asLeft)
        }
     
      case FeasibilityQuery.Delete(id) =>
        log.info(s"Deleting FeasibilityQuery $id by $querier")
        Future.successful(
          feasibilityQuerySessions.remove(id) match {
            case Some((query,_)) => query.asRight
            case None => Query.InvalidId.asLeft
          }
        )
    } 
  }


  def feasibilityQuery(
    id: Id[FeasibilityQuery]
  )(
    implicit ctx: ExecutionContext,
  ): Future[Option[FeasibilityQuery]] =
    Future.successful(
      feasibilityQuerySessions.get(id).map(_._1)
    )


  def aggregatedResults(
    id: Id[FeasibilityQuery]
  )(
    implicit ctx: ExecutionContext
  ): Future[Option[FeasibilityQuery.AggregatedResults]] =
    Future.successful(
      feasibilityQuerySessions.get(id).map(_._2)
    )


  // Suppress deprecation warning for CriteriaExpander for now
  @annotation.nowarn("cat=deprecation") 
  def process(
    request: FeasibilityQuery.Request
  )(
    implicit ctx: ExecutionContext
  ): Future[Either[String,request.ResultType]] = {

    log.info(
      s"""Processing feasibility query from site ${request.origin.code}, Querier: ${request.querier}, Criteria:\n${Json.prettyPrint(Json.toJson(request.criteria))}"""
    )

    localFeasibilityResults(request.criteria)
  }


  private def executeFeasibilityQuery(
    id: Id[FeasibilityQuery],
    mode: Query.Mode.Value,
    criteria: Criteria
  )(
    implicit
    ctx: ExecutionContext,
    querier: Querier
  ): Future[Map[Coding[Site],Either[String,FeasibilityQuery.LocalResults]]] = {

    import cats.syntax.apply._

    val externalResults =
      mode match {
        case Query.Mode.Federated =>
          connector ! (FeasibilityQuery.Request(Site.local,querier,criteria),connector.otherSites)

        case _ =>
          Future.successful(Map.empty[Coding[Site],Either[String,FeasibilityQuery.LocalResults]])
      }

    val localResults =
      localFeasibilityResults(criteria)
        .map(results => Some(Site.local -> results))

    (externalResults,localResults)
      .mapN(_ ++ _)

  }

  // Suppress deprecation warning for CriteriaExpander for now
  @annotation.nowarn("cat=deprecation") 
  private def localFeasibilityResults(
    criteria: Criteria
  )(
    implicit ctx: ExecutionContext
  ): Future[Either[String,FeasibilityQuery.LocalResults]] =

    // Expand the query criteria
    (db ? Some(CriteriaExpander(criteria)))
      .map(_.map(matches => FeasibilityQuery.LocalResults.of(Site.local,matches.map(_.record.data))))

}
