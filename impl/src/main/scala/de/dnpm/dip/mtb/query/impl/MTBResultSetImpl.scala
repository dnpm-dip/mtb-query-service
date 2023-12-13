package de.dnpm.dip.mtb.query.impl


import de.dnpm.dip.model.{
  Id,
  Patient,
  Snapshot
}
import de.dnpm.dip.service.query.{
  PatientFilter,
  PatientMatch,
  Query,
  BaseResultSet,
  Distribution,
  ReportingOps
}
import de.dnpm.dip.mtb.model.MTBPatientRecord
import de.dnpm.dip.mtb.query.api.{
  MTBQueryCriteria,
  MTBResultSet,
  MTBResultSummary,
}


class MTBResultSetImpl
(
  val id: Query.Id,
  val results: Seq[(Snapshot[MTBPatientRecord],MTBQueryCriteria)]
)
extends MTBResultSet
with BaseResultSet[MTBPatientRecord,MTBQueryCriteria]
with ReportingOps
{

  override def summary(
    filter: MTBPatientRecord => Boolean
  ) = {

    val records =
      results.collect {
        case (Snapshot(patRec,_),_) if (filter(patRec)) => patRec
      }

    val patients =
      records.map(_.patient)


    MTBResultSummary(
      id,
      records.size,
      MTBResultSummary.Distributions(
        DistributionOf(patients.map(_.gender)),
        AgeDistribution(patients.map(_.age)),
        DistributionOf(patients.flatMap(_.managingSite)),
      )
    )

  }

}
