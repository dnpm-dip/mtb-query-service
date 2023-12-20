package de.dnpm.dip.mtb.query.impl


//import java.time.temporal.ChronoUnit.WEEKS
import de.dnpm.dip.model.{
  Id,
  Duration,
  Patient,
  Snapshot,
}
import de.dnpm.dip.model.UnitOfTime.Weeks
import de.dnpm.dip.service.query.{
  Entry,
  PatientFilter,
  PatientMatch,
  Query,
  ResultSet,
  BaseResultSet,
  Distribution,
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
)
extends MTBResultSet
with BaseResultSet[MTBPatientRecord,MTBQueryCriteria]
with ReportingOps
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

        Summary(
          id,
          recs.size,
          ResultSet.Demographics.on(recs.map(_.patient)),
          TumorDiagnostics(
            DistributionOf(
              recs.flatMap(_.diagnoses.toList)
                .map(_.code)
            ),
            DistributionOf(
              recs.flatMap(_.getHistologyReports)
                .flatMap(_.results.tumorMorphology.map(_.value))
            )
          ),
          Medication(
            Medication.Recommendations(
              DistributionBy(
                recs.flatMap(_.getCarePlans.flatMap(_.medicationRecommendations))
                  .map(_.medication)
              )(
                _.flatMap(_.display)
              )
            ),
            Medication.Therapies(
              recs
                .flatMap(_.getMedicationTherapies)
                .flatMap(_.history.maxByOption(_.recordedOn))
                .filter(_.medication.isDefined)
                .groupBy(_.medication.get.flatMap(_.display))
                .map {
                  case (meds,therapies) =>
                    Entry(
                      meds,
                      (
                        therapies.size,
                        therapies
                          .flatMap(_.period.flatMap(_.duration(Weeks)))
                          .map(_.value)
                          .pipe(mean(_))
                          .pipe(Duration(_,Weeks))
                      )
                    )
                }
                .toSeq

            )
          )
        )

    }

}
