package de.dnpm.dip.mtb.query.impl 


import scala.concurrent.Future
import cats.{
  Applicative,
  Monad
}
import cats.data.EitherNel
import cats.syntax.either._
import de.dnpm.dip.util.Logging
import de.dnpm.dip.service.{
  Cache,
  Connector
}
import de.dnpm.dip.connector.{
  FakeConnector,
  HttpConnector,
  HttpMethod
}
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


  override def ResultSetFrom(
    query: Query[MTBQueryCriteria],
    results: Seq[Query.Match[MTBPatientRecord,MTBQueryCriteria]]
  ) =
    new MTBResultSetImpl(query.id,query.criteria,results)


  override def validate(
    criteria: MTBQueryCriteria
  ): EitherNel[String,MTBQueryCriteria] =
    //TODO: Adapt to check that criteria be non-empty after having announced this as a breaking change
    criteria.asRight 
    

  override val survivalConfig: KaplanMeier.Config =
    kmModule.survivalConfig

}
