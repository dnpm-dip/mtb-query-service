package de.dnpm.dip.mtb.query.api


import scala.concurrent.Future
import cats.Monad
import de.dnpm.dip.util.{
  SPI,
  SPILoader
}
import de.dnpm.dip.service.query.QueryService
import de.dnpm.dip.mtb.model.MTBPatientRecord



trait MTBQueryService extends QueryService[
  Future,
  Monad[Future],
  MTBConfig
//  MTBPatientRecord,
//  MTBQueryCriteria,
//  MTBResultSet
]
{
  def survivalConfig: KaplanMeier.Config
}

trait MTBQueryServiceProvider extends SPI[MTBQueryService]

object MTBQueryService extends SPILoader[MTBQueryServiceProvider]

