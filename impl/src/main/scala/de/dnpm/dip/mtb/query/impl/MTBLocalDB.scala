package de.dnpm.dip.mtb.query.impl


import java.io.File
import scala.util.Try
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

  private[impl] val mtbDataDirProp =
    "dnpm.dip.mtb.query.data.dir"

  private[impl] val dataDirProp =
    "dnpm.dip.query.data.dir"


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
        }
        .getOrElse(
          Option(System.getProperty(mtbDataDirProp))
            .orElse(
              Option(System.getProperty(dataDirProp)).map(dir => s"$dir/mtb_data")
            ) match {
    
            case Some(dir) =>
              new FSBackedLocalDB[Future,Monad,MTBQueryCriteria,MTBPatientRecord](
                new File(dir),
                matcher
              )
    
            case None =>
              val msg =
                s"System property $dataDirProp or $mtbDataDirProp for the data storage directory is undefined, but random data generation is also not activated!"
                 .tap(log.error) 
              throw new IllegalStateException(msg)
          }
        )
    
      }

}
