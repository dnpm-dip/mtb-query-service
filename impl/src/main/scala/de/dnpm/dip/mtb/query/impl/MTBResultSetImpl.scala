package de.dnpm.dip.mtb.query.impl


import java.time.temporal.ChronoUnit.DAYS
import de.dnpm.dip.model.UnitOfTime
import scala.math.round
import cats.{
  Applicative,
  Id
}
import de.dnpm.dip.coding.{
  Coding,
  CodeSystem,
  CodeSystemProvider
}
import de.dnpm.dip.coding.atc.ATC
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
import de.dnpm.dip.mtb.query.api.KaplanMeier._


class MTBResultSetImpl
(
  val id: Query.Id,
  val results: Seq[(Snapshot[MTBPatientRecord],MTBQueryCriteria)]
)(
  implicit
  hgnc: CodeSystem[HGNC],
  atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
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
    results
      .collect { case (snp,_) if f(snp.data) => snp }
      .pipe {
        snps =>

        val recs = snps.map(_.data)

        val (therapyCounts,meanTherapyDurations) =  
          therapyCountsWithMeanDuration(recs)

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
          ),
/*        
          CombinedSurvivalStatistics(
            KaplanMeierOps.overallSurvival(
              snps,
              DAYS
            )
            .map { case (group,cohortData) => Entry(group,cohortData) }
            .toSeq
            .pipe(
              SurvivalStatistics(UnitOfTime.of(DAYS),_)
            ),
            KaplanMeierOps.progressionFreeSurvival(
              snps,
              DAYS
            )
            .map { case (group,cohortData) => Entry(group,cohortData) }
            .toSeq
            .pipe(
              SurvivalStatistics(UnitOfTime.of(DAYS),_)
            )
          )
*/        
        )

    }

/*    
  override def summary(
    f: MTBPatientRecord => Boolean
  ): Summary =
    records
      .filter(f)
      .pipe {
        recs =>

        val (therapyCounts,meanTherapyDurations) =  
          therapyCountsWithMeanDuration(recs)

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
*/
}
