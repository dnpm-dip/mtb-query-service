package de.dnpm.dip.mtb.query.impl


import scala.math.round
import de.dnpm.dip.coding.{
  Coding,
  CodeSystem
}
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.model.Snapshot
import de.dnpm.dip.service.query.{
  Entry,
  PatientFilter,
  PatientMatch,
  Query,
  ResultSet,
  BaseResultSet,
  ReportingOps
}
import de.dnpm.dip.mtb.model.MTBPatientRecord
import de.dnpm.dip.mtb.query.api.{
  MTBQueryCriteria,
  MTBResultSet,
}


class MTBResultSetImpl
(
  val id: Query.Id,
  val results: Seq[(Snapshot[MTBPatientRecord],MTBQueryCriteria)]
)(
  implicit hgnc: CodeSystem[HGNC]
)  
extends MTBResultSet
with BaseResultSet[MTBPatientRecord,MTBQueryCriteria]
with MTBReportingOps
{

  import scala.util.chaining._
  import MTBResultSet.{
    Summary,
    TumorDiagnostics,
    Medication
  }

  override def summary(
    f: MTBPatientRecord => Boolean
  ): Summary =
    records
      .filter(f)
      .pipe {
        recs =>

        val (therapyCounts,meanTherapyDurations) =  
          therapyCountsWithMeanDuration(records)

        Summary(
          id,
          recs.size,
          ResultSet.Demographics.on(recs.map(_.patient)),
          TumorDiagnostics(
            distribution(
              recs.flatMap(_.diagnoses.toList)
                .map(_.code)
            ),
            tumorEntitiesByVariant(records),
            distribution(
              recs.flatMap(_.getHistologyReports)
                .flatMap(_.results.tumorMorphology.map(_.value))
            )
          ),
          Medication(
            Medication.Recommendations(
              distributionBy(
                recs
                  .flatMap(
                    _.getCarePlans.flatMap(_.medicationRecommendations)
                  )
                  .map(_.medication)
              )(
                _.flatMap(_.display)
              ),
              recommendationsBySupportingVariant(records)
            ),
            Medication.Therapies(
              therapyCounts,
              meanTherapyDurations,
              responsesByTherapy(records)  
            )
          )
        )

    }

}
