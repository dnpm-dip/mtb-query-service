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
import de.dnpm.dip.service.query.{
  BaseQueryService,
  Connector,
  Filters,
  Data,
  Query,
  Querier,
  QueryCache,
  BaseQueryCache,
  PatientFilter,
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
import de.dnpm.dip.connector.{
  FakeConnector,
  HttpConnector
}



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
    System.getProperty("dnpm.dip.connector.type","peer2peer") match {
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


  private val db =
    new InMemLocalDB[Future,Monad,MTBQueryCriteria,MTBPatientRecord](
      MTBQueryCriteriaOps.criteriaMatcher(strict = true)
    )
    with MTBLocalDB


  private[impl] lazy val instance =
    new MTBQueryServiceImpl(
      new InMemPreparedQueryDB[Future,Monad,MTBQueryCriteria],
      db,
      connector,
      cache
    )

  Try(
    Option(System.getProperty("dnpm.dip.mtb.query.data.generate")).get
  )
  .map(_.toInt)
  .foreach {
    n =>

      import de.ekut.tbi.generators.Gen
      import de.dnpm.dip.mtb.gens.Generators._
      import scala.util.Random
      import scala.concurrent.ExecutionContext.Implicits.global

      implicit val rnd: Random =
        new Random

      for (i <- 0 until n){
        instance ! Data.Save(Gen.of[MTBPatientRecord].next)
      }
  }
    
}


class MTBQueryServiceImpl
(
  val preparedQueryDB: PreparedQueryDB[Future,Monad[Future],MTBQueryCriteria,String],
  val db: MTBLocalDB,
  val connector: Connector[Future,Monad[Future]],
  val cache: QueryCache[MTBQueryCriteria,MTBFilters,MTBResultSet,MTBPatientRecord]
)
extends BaseQueryService[
  Future,
  MTBConfig
]
with MTBQueryService
with Completers
{

  import Completer.syntax._    


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


  import scala.language.implicitConversions

  override implicit def toPredicate(filter: MTBFilters): MTBPatientRecord => Boolean = {
    record =>

      implicit def diagnosisFilterPredicate(f: DiagnosisFilter): MTBDiagnosis => Boolean =
        diag =>
          f.code match {
            case Some(icd10s) if icd10s.nonEmpty => icd10s.exists(_.code == diag.code.code)
            case _                               => true
          }


      filter.patientFilter(record.patient) &&
      record.getDiagnoses.exists(filter.diagnosisFilter)

  }


  override val localSite: Coding[Site] =
    connector.localSite
      
    
  override implicit val hgnc: CodeSystem[HGNC] =
    HGNC.GeneSet
      .getInstance[cats.Id]
      .get
      .latest

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
//    DefaultKaplanMeierModule


  override val ResultSetFrom =
    new MTBResultSetImpl(_,_)

  //TODO: Complete codings, etc
  override val preprocess: MTBPatientRecord => MTBPatientRecord =
    _.complete


}
