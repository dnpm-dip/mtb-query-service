package de.dnpm.dip.mtb.query.impl 



import java.io.File
import scala.concurrent.Future
import scala.util.{
  Try,
  Failure
}
import cats.Monad
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
  QueryCache,
  BaseQueryCache,
  PatientFilter,
  InMemLocalDB,
  PreparedQueryDB,
  InMemPreparedQueryDB,
}
import de.dnpm.dip.coding.{
  Coding,
  CodeSystem
}
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.mtb.model.MTBPatientRecord
import de.dnpm.dip.mtb.query.api._
import de.dnpm.dip.connector.peer2peer.PeerToPeerConnector



class MTBQueryServiceProviderImpl
extends MTBQueryServiceProvider
{

  override def getInstance: MTBQueryService =
    return MTBQueryServiceImpl.instance

}


object MTBQueryServiceImpl extends Logging
{

  private val cache =
    new BaseQueryCache[MTBQueryCriteria,MTBFilters,MTBResultSet,MTBPatientRecord]


  private val connector =
    PeerToPeerConnector(
      "/api/mtb/peer2peer/",
      PartialFunction.empty
    )


  private val db =
    new InMemLocalDB[Future,Monad,MTBQueryCriteria,MTBPatientRecord](
      ???
//      MTBQueryCriteriaOps.criteriaMatcher(strict = true)
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
    Option(System.getProperty("dnpm.dip.rd.query.data.generate")).get
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



  override val ResultSetFrom =
    new MTBResultSetImpl(_,_)


  override def DefaultFilter(
    results: Seq[Snapshot[MTBPatientRecord]]
  ): MTBFilters = {

    val records =
      results.map(_.data)

    MTBFilters(
      PatientFilter.on(records),
    )
  }


  override val localSite: Coding[Site] =
    connector.localSite
      
/*
  override implicit val hgnc: CodeSystem[HGNC] =
    HGNC.GeneSet
      .getInstance[cats.Id]
      .get
      .latest
*/
        

  //TODO: Complete codings, etc
  override val preprocess: MTBPatientRecord => MTBPatientRecord =
    _.complete


}
