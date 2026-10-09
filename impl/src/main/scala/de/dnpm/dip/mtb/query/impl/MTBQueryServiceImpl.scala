package de.dnpm.dip.mtb.query.impl 


import scala.concurrent.{
  ExecutionContext,
  Future
}
import cats.{
  Id,
  Applicative,
  Monad
}
import cats.data.EitherNel
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.ior._
import de.dnpm.dip.util.Logging
import de.dnpm.dip.service.{
  ConnectionStatus,
  Connector,
  Entry
}
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.{
  Site,
  UnitOfTime
 }
import UnitOfTime.Days
import de.dnpm.dip.connector.{
  FakeConnector,
  HttpConnector,
  HttpMethod
}
import HttpConnector.QueryParameters
import HttpConnector.QueryParameters._
import de.dnpm.dip.service.query.{
  BaseQueryService,
  Query,
  QueryCache,
  BaseQueryCache,
  FederatedQuery,
  PatientRecordRequest,
  LocalDB,
  PreparedQueryDB
}
import de.dnpm.dip.coding.CodeSystemProvider
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.{
  ICD10GM,
  ICDO3
}
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.mtb.model.MTBPatientRecord
import de.dnpm.dip.mtb.query.api._
import KaplanMeier.{
  GlobalSurvivalStatistics,
  Grouping,
  RawSurvivalStatistics,
  RawSurvivalStatisticsRequest,
  RawDataPoint,
  SurvivalType
}


class MTBQueryServiceProviderImpl extends MTBQueryServiceProvider
{

  override def getInstance: MTBQueryService =
    return MTBQueryServiceImpl.instance

}


object MTBQueryServiceImpl extends Logging
{

  import HttpMethod._

  private val cache =
    new BaseQueryCache[MTBQueryCriteria,MTBResultSet,MTBPatientRecord]

  private val federatedQueriesActive =
    sys.env.get("ACTIVE_FEDERATED_QUERY_USE_CASES")
      .map(_.split(",").map(_.trim.toUpperCase).toSet)
      .exists(_ contains "MTB")


  private lazy val connector =
    System.getProperty(HttpConnector.Type.property,"broker") match {
      case HttpConnector.Type(typ) =>
        val baseURI = "/api/mtb/peer2peer"
        HttpConnector(
          typ,
          { 
            case _: FederatedQuery[_,_] =>
              (POST, s"$baseURI/query", Map.empty)

            case PatientRecordRequest(_,querier,patient,snapshot) =>
              (
                GET, s"$baseURI/patient-record", QueryParameters(
                  "querier" -> querier.value,
                  "patient" -> patient.value
                  ) + ("snapshot" -> snapshot.map(_.toString))
              )

            case RawSurvivalStatisticsRequest(_,survivalType,grouping,timeUnit) =>
              (
                GET, s"$baseURI/raw-survival-statistics", Seq(
                  "type"     -> survivalType.map(_.toString),
                  "grouping" -> grouping.map(_.toString),
                  "timeunit" -> timeUnit.map(_.toString)
                )
                .foldLeft(Map.empty[String,Seq[String]])((acc,param) => acc + param)
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
      cache,
      federatedQueriesActive
    )
}


class MTBQueryServiceImpl
(
  val preparedQueryDB: PreparedQueryDB[Future,Monad[Future],MTBQueryCriteria,String],
  val db: LocalDB[Future,Monad[Future],MTBQueryCriteria,MTBPatientRecord],
  val connector: Connector[Future,Monad[Future]],
  val cache: QueryCache[MTBQueryCriteria,MTBResultSet,MTBPatientRecord],
  val federatedQueriesActive: Boolean
)
extends BaseQueryService[Future,MTBConfig]
with MTBQueryService
with Completers
{

    
  override implicit val hgnc: CodeSystemProvider[HGNC,Id,Applicative[Id]] =
    HGNC.GeneSet
      .getInstance[cats.Id]
      .get

  override implicit val atc: CodeSystemProvider[ATC,Id,Applicative[Id]] =
    ATC.Catalogs
      .getInstance[cats.Id]
      .get


  override implicit val icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]] =
    ICD10GM.Catalogs
      .getInstance[cats.Id]
      .get

  override implicit val icdo3: ICDO3.Catalogs[Id,Applicative[Id]] =
    ICDO3.Catalogs  
      .getInstance[cats.Id]
      .get



  private implicit val kmEstimator: KaplanMeierEstimator[Id] =
    DefaultKaplanMeierEstimator

  private implicit val kmModule: KaplanMeierModule[Id] =
    new DefaultKaplanMeierModule


  @annotation.nowarn // To suppress deprecation warning for CriteriaExpander
  override def ResultSetFrom(
    query: Query[MTBQueryCriteria],
    results: Seq[Query.Match[MTBPatientRecord,MTBQueryCriteria]]
  ) =
    new MTBResultSetImpl(
      query.id,
      query.criteria.map(CriteriaExpander),
      results
    )


  override val survivalConfig: KaplanMeier.Config =
    kmModule.survivalConfig


  override def !(
    request: RawSurvivalStatisticsRequest
  )(
    implicit env: ExecutionContext
  ): Future[Either[String,RawSurvivalStatistics]] = {

    val RawSurvivalStatisticsRequest(origin,survivalType,grouping,timeUnit) = request

    log.info(s"Processing RawSurvivalStatistics request - Origin: $origin, Type: $survivalType, Grouping: $grouping")

    rawSurvivalStatistics(survivalType,grouping,timeUnit)  
  }


  private def rawSurvivalStatistics(
    survivalType: Option[SurvivalType.Value],
    grouping: Option[Grouping.Value],
    timeUnit: Option[UnitOfTime]
  )(        
    implicit env: ExecutionContext
  ): Future[Either[String,RawSurvivalStatistics]] =

    //TODO: Cache to avoid multiple re-compilation of results on successive requests
    for { 
  
      matches <- db ? (criteria = None)
  
      snapshots = matches.map(_.map(_.record))
  
      result = snapshots.map(kmModule.rawSurvivalStatistics(survivalType,grouping,timeUnit,_) )
  
    } yield result  


  override def survivalStatistics(
    survivalType: Option[SurvivalType.Value],
    grouping: Option[Grouping.Value]
  )(
    implicit env: ExecutionContext
  ): Future[Either[KaplanMeier.Error,GlobalSurvivalStatistics]] = {

    log.info(s"Compiling GlobalSurvivalStatistics - Type: $survivalType, Grouping: $grouping")

    val timeUnit = Days

    for {
      resultsBySite <- (
        connector ! RawSurvivalStatisticsRequest(Site.local,survivalType,grouping,Some(timeUnit)),
        rawSurvivalStatistics(survivalType,grouping,Some(timeUnit))
          .map(result => Some(Site.local -> result))
      )
      .mapN(
        (externalResultsBySite,localResult) => externalResultsBySite ++ localResult
      )

      combinedResults: EitherNel[String,Seq[RawDataPoint]] =
        resultsBySite.values
          .map(_.map(_.data).toIor.toIorNel)
          .reduceOption(_ combine _)
          .getOrElse(Seq.empty.rightIor)
          .toEither

      outcome = combinedResults match { 

        case Right(dataPoints) if dataPoints.nonEmpty => 
          GlobalSurvivalStatistics(
            Coding(survivalType.getOrElse(survivalConfig.defaults.`type`)),
            Coding(grouping.getOrElse(survivalConfig.defaults.grouping)),
            timeUnit,
            dataPoints.groupMap(_.groupLabel)(dataPoint => dataPoint.time -> dataPoint.event)
              .map { case (group,data) => Entry(group,kmEstimator.cohortResult(data)) }
              .toSeq,
            ConnectionStatus.from(resultsBySite)
          )
          .asRight

        case Right(_) => KaplanMeier.NoResults.asLeft

        case Left(errors) => KaplanMeier.ConnectionErrors(errors).asLeft
      }

    } yield outcome

  }

}
