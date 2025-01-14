package de.dnpm.dip.mtb.query.impl


import java.io.File
import scala.concurrent.Future
import cats.Monad
import de.dnpm.dip.service.query.{
  PreparedQueryDB,
  FSBackedPreparedQueryDB,
  InMemPreparedQueryDB
}
import de.dnpm.dip.mtb.query.api.MTBQueryCriteria
import de.dnpm.dip.util.Logging


trait MTBPreparedQueryDB extends PreparedQueryDB[
  Future,
  Monad[Future],
  MTBQueryCriteria,
  String
]

object MTBPreparedQueryDB extends Logging
{

  private[impl] val dataDirProp =
    "dnpm.dip.data.dir"

  lazy val instance: PreparedQueryDB[Future,Monad[Future],MTBQueryCriteria,String] =
    Option(System.getProperty(dataDirProp))
      .map( dir =>
        new FSBackedPreparedQueryDB[Future,Monad,MTBQueryCriteria](
          new File(s"$dir/mtb_data/prepared_queries")
        )
      )
      .getOrElse {
        log.warn(
          s"System property $dataDirProp for the data storage directory is undefined. Falling back to in-memory store for MTB Prepared Queries!"
        )
        new InMemPreparedQueryDB[Future,Monad,MTBQueryCriteria]
      }   

}
