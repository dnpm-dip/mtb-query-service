package de.dnpm.dip.mtb.query.api


import scala.concurrent.Future
import cats.Monad
import de.dnpm.dip.util.{
  SPI,
  SPILoader
}
import de.dnpm.dip.service.query.QueryService



trait MTBQueryService extends QueryService[
  Future,
  Monad[Future],
  MTBConfig,
  String
]


trait MTBQueryServiceProvider extends SPI[MTBQueryService]

object MTBQueryService extends SPILoader[MTBQueryServiceProvider]

