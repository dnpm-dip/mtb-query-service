package de.dnpm.dip.mtb.query.api


import scala.concurrent.{
  ExecutionContext,
  Future
}
import cats.Monad
import de.dnpm.dip.util.{
  SPI,
  SPILoader
}
import de.dnpm.dip.service.query.QueryService


trait MTBQueryService
extends QueryService[Future,Monad[Future],MTBConfig]
with FeasibilityQuery.Operations[Future,ExecutionContext]
{
  def survivalConfig: KaplanMeier.Config
}

trait MTBQueryServiceProvider extends SPI[MTBQueryService]

object MTBQueryService extends SPILoader[MTBQueryServiceProvider]

