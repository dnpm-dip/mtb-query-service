package de.dnpm.dip.mtb.query.impl


import scala.concurrent.Future
import cats.Monad
import de.dnpm.dip.mtb.model.MTBPatientRecord
import de.dnpm.dip.mtb.query.api.MTBQueryCriteria
import de.dnpm.dip.service.query.InMemLocalDB



class InMemMTBLocalDB
(
  strict: Boolean
)
extends InMemLocalDB[
  Future,
  Monad,
  MTBQueryCriteria,
  MTBPatientRecord
](
  MTBQueryCriteriaOps.criteriaMatcher(strict = strict)
)
with MTBLocalDB

