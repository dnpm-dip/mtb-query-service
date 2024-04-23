package de.dnpm.dip.mtb.query.impl


import java.io.File
import scala.util.Try
import scala.util.Random
import scala.util.chaining._
import scala.concurrent.Future
import cats.Monad
import de.dnpm.dip.model.{
  Id,
  Patient,
  Snapshot,
  Site
}
import de.dnpm.dip.service.query.{
  LocalDB,
  FSBackedLocalDB,
  InMemLocalDB,
}
import de.dnpm.dip.mtb.model.MTBPatientRecord
import de.dnpm.dip.mtb.query.api.MTBQueryCriteria
import de.dnpm.dip.util.{
  SPI,
  SPILoader
}
import de.ekut.tbi.generators.Gen
import de.dnpm.dip.mtb.gens.Generators._
import de.dnpm.dip.mtb.model.Completers._
import de.dnpm.dip.util.Completer.syntax._


trait MTBLocalDB extends LocalDB[
  Future,
  Monad[Future],
  MTBQueryCriteria,
  MTBPatientRecord
]

trait MTBLocalDBSPI extends SPI[MTBLocalDB]

object MTBLocalDB extends SPILoader[MTBLocalDBSPI]
{

  private[impl] val dataGenProp =
    "dnpm.dip.mtb.query.data.generate"

  private[impl] val dataDirProp =
    "dnpm.dip.data.dir"


  lazy val instance: LocalDB[Future,Monad[Future],MTBQueryCriteria,MTBPatientRecord] =
    getInstance
      .getOrElse {

        val matcher =
          MTBQueryCriteriaOps.criteriaMatcher(strict = true)
    
        Try(
          System.getProperty(dataGenProp).toInt
        )
        .collect {
          case n if n >= 0 =>
            log.warn(s"Random data generation activated, using in-memory DB only!")

            new InMemLocalDB[Future,Monad,MTBQueryCriteria,MTBPatientRecord](matcher)
              .tap {
                db =>

                import scala.concurrent.ExecutionContext.Implicits.global
              
                implicit val rnd: Random = new Random
            
                for (i <- 0 until n){
                  db.save(Gen.of[MTBPatientRecord].next.complete)
                }
              }
        }
        .getOrElse(        
          Option(System.getProperty(dataDirProp)).map(dir => s"$dir/mtb_data/query") match {
    
            case Some(dir) =>
              new FSBackedLocalDB[Future,Monad,MTBQueryCriteria,MTBPatientRecord](
                new File(dir),
                matcher
              )
    
            case None =>
              val msg =
                s"System property $dataDirProp for the data storage directory is undefined, but random data generation is also not activated!"
                 .tap(log.error) 
              throw new IllegalStateException(msg)
          }
        )
    
      }

}
