package de.dnpm.dip.mtb.query.impl 



import java.io.File
import scala.concurrent.Future
import scala.util.{
  Try,
  Failure
}
import cats.{
  Id,
  Applicative,
  Monad
}
import de.dnpm.dip.util.{
  Completer,
  Logging
}
import de.dnpm.dip.model.{
  ClosedInterval,
  Site,
  Snapshot,
  Patient
}
import de.dnpm.dip.service.Connector
import de.dnpm.dip.service.Data.Save
import de.dnpm.dip.connector.{
  FakeConnector,
  HttpConnector
}
import de.dnpm.dip.service.query.{
  BaseQueryService,
  Entry,
  Filters,
  Query,
  Querier,
  QueryCache,
  BaseQueryCache,
  PatientFilter,
  LocalDB,
  FSBackedLocalDB,
  InMemLocalDB,
  PreparedQueryDB,
  InMemPreparedQueryDB,
}
import de.dnpm.dip.coding.{
  Coding,
  CodeSystem,
  CodeSystemProvider
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.{
  ICD10GM,
  ICDO3
}
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.mtb.model.{
  MTBDiagnosis,
  MTBPatientRecord
}
import de.dnpm.dip.mtb.query.api._



class MTBQueryServiceProviderImpl extends MTBQueryServiceProvider
{

  override def getInstance: MTBQueryService =
    return MTBQueryServiceImpl.instance

}


object MTBQueryServiceImpl extends Logging
{

  private val cache =
    new BaseQueryCache[MTBQueryCriteria,MTBFilters,MTBResultSet,MTBPatientRecord]


  private lazy val connector =
    System.getProperty(HttpConnector.Type.property,"broker") match {
      case HttpConnector.Type(typ) =>
        HttpConnector(
          typ,
          "/api/mtb/peer2peer/",
          PartialFunction.empty
        )

      case _ =>
        import scala.concurrent.ExecutionContext.Implicits._
        log.warn("Falling back to Fake Connector!")
        FakeConnector[Future]
    }

  private[impl] lazy val instance =
    new MTBQueryServiceImpl(
      new InMemPreparedQueryDB[Future,Monad,MTBQueryCriteria],  //TODO: change to persistent Prepared Query store
      MTBLocalDB.instance,
      connector,
      cache
    )
}


class MTBQueryServiceImpl
(
  val preparedQueryDB: PreparedQueryDB[Future,Monad[Future],MTBQueryCriteria,String],
  val db: LocalDB[Future,Monad[Future],MTBQueryCriteria,MTBPatientRecord],
  val connector: Connector[Future,Monad[Future]],
  val cache: QueryCache[MTBQueryCriteria,MTBFilters,MTBResultSet,MTBPatientRecord]
)
extends BaseQueryService[Future,MTBConfig]
with MTBQueryService
with Completers
{


  import scala.language.implicitConversions

  override implicit def filterToPredicate(filter: MTBFilters): MTBPatientRecord => Boolean = {

    implicit def diagnosisFilterPredicate(f: DiagnosisFilter): MTBDiagnosis => Boolean =
      diag =>
        f.code match {
          case Some(icd10s) if icd10s.nonEmpty => icd10s.exists(_.code == diag.code.code)
          case _                               => true
        }

    record =>
      filter.patientFilter(record.patient) &&
      record.getDiagnoses.exists(filter.diagnosisFilter)
  }


  override def DefaultFilter(
    results: Seq[Snapshot[MTBPatientRecord]]
  ): MTBFilters = {

    val records =
      results.map(_.data)

    MTBFilters(
      PatientFilter.on(records),
      DiagnosisFilter(
        Some(records.flatMap(_.getDiagnoses.map(_.code)).toSet)
      )
    )
  }

    
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


  import MTBQueryCriteriaOps._

  override val ResultSetFrom =
    (id,criteria,results) =>
      new MTBResultSetImpl(
        id,
        criteria,
        results.map { 
          case (snp,criteria) => snp -> Some(criteria).filterNot(_.isEmpty)
        }
      )


  override val survivalConfig: KaplanMeier.Config =
    kmModule.survivalConfig

}
