package de.dnpm.dip.mtb.query.impl 


import scala.concurrent.Future
import cats.{
  Id,
  Applicative,
  Monad
}
import de.dnpm.dip.util.Logging
import de.dnpm.dip.service.Connector
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

}
