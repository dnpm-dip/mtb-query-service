package de.dnpm.dip.mtb.query.impl


import java.io.File
import scala.util.Either
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
  FSBackedLocalDB
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



/*
class FSBackedMTBLocalDB(
  dataDir: File
)
extends FSBackedLocalDB[
  Future,
  Monad,
  MTBQueryCriteria,
  MTBPatientRecord
](
  dataDir,
  "MTBPatientRecord",
  MTBQueryCriteriaOps.criteriaMatcher(strict = true),
)
with MTBLocalDB
*/
