package de.dnpm.dip.mtb.query.impl 


import scala.concurrent.{
  ExecutionContext,
  Future
}
import scala.concurrent.duration._
import scala.util.Success
import cats.{
  Applicative,
  Monad
}
import cats.data.{
  EitherNel,
  Ior
}
import cats.syntax.apply._
import cats.syntax.either._
//import cats.syntax.ior._
import de.dnpm.dip.util.Logging
import de.dnpm.dip.service.{
  Cache,
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
//  RawDataPoint,
  RawSurvivalStatistics,
  RawSurvivalStatisticsRequest,
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

            case RawSurvivalStatisticsRequest(_,survivalTypeAndGrouping,timeUnit) =>
              (
                GET, s"$baseURI/raw-survival-statistics", Seq(
                  "type"     -> survivalTypeAndGrouping.map(_._1.toString),
                  "grouping" -> survivalTypeAndGrouping.flatMap(_._2).map(_.toString),
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
      federatedQueriesActive
    )
}


class MTBQueryServiceImpl
(
  val preparedQueryDB: PreparedQueryDB[Future,Monad[Future],MTBQueryCriteria,String],
  val db: LocalDB[Future,Monad[Future],MTBQueryCriteria,MTBPatientRecord],
  val connector: Connector[Future,Monad[Future]],
  val federatedQueriesActive: Boolean
)
extends BaseQueryService[Future,MTBConfig]
with MTBQueryService
with Completers
{

  override val querySessions = Cache.empty()

  private val survivalStatisticsResults = Cache.empty[(SurvivalType.Value,Grouping.Value),RawSurvivalStatistics]()
    
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


  override def validate(
    criteria: MTBQueryCriteria
  ): EitherNel[String,MTBQueryCriteria] =
    //TODO: Adapt to check that criteria be non-empty after having announced this as a breaking change
    criteria.asRight 
    

  override val survivalConfig: KaplanMeier.Config =
    kmModule.survivalConfig

  private def valuesOrDefaults(
    survivalTypeAndGrouping: Option[(SurvivalType.Value,Option[Grouping.Value])]
  ): (SurvivalType.Value,Grouping.Value) =
    survivalTypeAndGrouping match { 

      case Some(survivalType -> grouping) =>
        survivalType -> grouping.getOrElse(
          survivalType match { 
            case SurvivalType.OS  => Grouping.ObtainedTherapy
            case SurvivalType.PFS => Grouping.Therapy
          }
        )

      case _ => survivalConfig.defaults.`type` -> survivalConfig.defaults.grouping
       
    }


  override def !(
    request: RawSurvivalStatisticsRequest
  )(
    implicit env: ExecutionContext
  ): Future[Either[String,RawSurvivalStatistics]] = {

    val RawSurvivalStatisticsRequest(origin,survivalTypeAndGrouping,timeUnit) = request

    val (survivalType,grouping) = valuesOrDefaults(survivalTypeAndGrouping)

    log.info(s"Processing RawSurvivalStatistics request - Origin: $origin, Type: $survivalType, Grouping: $grouping")

    rawSurvivalStatistics(survivalType,grouping,timeUnit)  
  }


  override def globalSurvivalStatistics(
    survivalTypeAndGrouping: Option[(SurvivalType.Value,Option[Grouping.Value])],
  )(
    implicit env: ExecutionContext
  ): Future[Either[KaplanMeier.Error,GlobalSurvivalStatistics]] = {

    val (survivalType,grouping) = valuesOrDefaults(survivalTypeAndGrouping)
 
    log.info(s"Compiling GlobalSurvivalStatistics - Type: $survivalType, Grouping: $grouping")

    val timeUnit = Days

    for {
      resultsBySite <- (
        connector ! RawSurvivalStatisticsRequest(Site.local,survivalTypeAndGrouping,Some(timeUnit)),
        rawSurvivalStatistics(survivalType,grouping,Some(timeUnit))
          .map(result => Some(Site.local -> result))
      )
      .mapN(
        (externalResultsBySite,localResult) => externalResultsBySite ++ localResult
      )

      outcome = 
        resultsBySite.values
          .map(_.map(_.data).toIor.toIorNel)
          .reduceOption(_ combine _)
          .map(
            _.map(
              dataPoints => Option.when(dataPoints.nonEmpty)(
                GlobalSurvivalStatistics(
                  Coding(survivalType),
                  Coding(grouping),
                  timeUnit,
                  dataPoints.groupMap(_.groupLabel)(dataPoint => dataPoint.time -> dataPoint.event)
                    .map { case (group,data) => Entry(group,kmEstimator.cohortResult(data)) }
                    .toSeq,
                  ConnectionStatus.from(resultsBySite)
                )
              )
            )
          )
          .collect {
            case Ior.Right(Some(globalResults))  => globalResults.asRight
            case Ior.Both(_,Some(globalResults)) => globalResults.asRight
            case Ior.Left(errors)                => KaplanMeier.ConnectionErrors(errors).asLeft
          }
          .getOrElse(KaplanMeier.NoResults.asLeft)

    } yield outcome

  }

/*
  override def globalSurvivalStatistics(
    survivalTypeAndGrouping: Option[(SurvivalType.Value,Option[Grouping.Value])],
  )(
    implicit env: ExecutionContext
  ): Future[Either[KaplanMeier.Error,GlobalSurvivalStatistics]] = {

    val (survivalType,grouping) = valuesOrDefaults(survivalTypeAndGrouping)
 
    log.info(s"Compiling GlobalSurvivalStatistics - Type: $survivalType, Grouping: $grouping")

    val timeUnit = Days

    for {
      resultsBySite <- (
        connector ! RawSurvivalStatisticsRequest(Site.local,survivalTypeAndGrouping,Some(timeUnit)),
        rawSurvivalStatistics(survivalType,grouping,Some(timeUnit))
          .map(result => Some(Site.local -> result))
      )
      .mapN(
        (externalResultsBySite,localResult) => externalResultsBySite ++ localResult
      )

      combinedResults =
        resultsBySite.values
          .map(_.map(_.data).toIor.toIorNel)
          .reduceOption(_ combine _)
          .getOrElse(Seq.empty.rightIor.toIorNel)

      outcome = combinedResults.map(
        dataPoints => Option.when(dataPoints.nonEmpty)(
          GlobalSurvivalStatistics(
            Coding(survivalType),
            Coding(grouping),
            timeUnit,
            dataPoints.groupMap(_.groupLabel)(dataPoint => dataPoint.time -> dataPoint.event)
              .map { case (group,data) => Entry(group,kmEstimator.cohortResult(data)) }
              .toSeq,
            ConnectionStatus.from(resultsBySite)
          )
        )
      ) match { 
        case Ior.Right(globalResults)  => globalResults.toRight(KaplanMeier.NoResults)
        case Ior.Both(_,globalResults) => globalResults.toRight(KaplanMeier.NoResults)
        case Ior.Left(errors)          => KaplanMeier.ConnectionErrors(errors).asLeft
      }

    } yield outcome

  }
*/

  private def rawSurvivalStatistics(
    survivalType: SurvivalType.Value,
    grouping: Grouping.Value,
    timeUnit: Option[UnitOfTime]
  )(        
    implicit env: ExecutionContext
  ): Future[Either[String,RawSurvivalStatistics]] =
    survivalStatisticsResults.get(survivalType -> grouping) match {

      case Some(result) => Future.successful(result.asRight)

      case _ => (
        for { 
        
          matches <- db ? (criteria = None)
        
          snapshots = matches.map(_.map(_.record))
        
          result = snapshots.map(
            kmModule.rawSurvivalStatistics(survivalType,grouping,timeUnit,_)
          )
        
        } yield result
      )
      .andThen { 
        case Success(Right(result)) => survivalStatisticsResults.put(survivalType -> grouping, result, 60 seconds)
      }
    }  

}
