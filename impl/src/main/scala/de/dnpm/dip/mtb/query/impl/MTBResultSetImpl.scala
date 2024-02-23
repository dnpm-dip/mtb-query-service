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
import de.dnpm.dip.coding.icd.{
  ICD10GM,
  ICDO3
}
import de.dnpm.dip.model.Snapshot
import de.dnpm.dip.service.query.{
  Distribution,
  Entry,
  PatientFilter,
  PatientMatch,
  Query,
  ResultSet,
  ReportingOps
}
import de.dnpm.dip.mtb.model.MTBPatientRecord
import de.dnpm.dip.mtb.query.api.{
  MTBQueryCriteria,
  MTBResultSet,
  KaplanMeier
}


class MTBResultSetImpl
(
  val id: Query.Id,
  val results: Seq[(Snapshot[MTBPatientRecord],MTBQueryCriteria)]
)(
  implicit
  hgnc: CodeSystem[HGNC],
  atc: CodeSystemProvider[ATC,Id,Applicative[Id]],
  icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]],
  icdo3: CodeSystemProvider[ICDO3,Id,Applicative[Id]],
  kmEstimator: KaplanMeierEstimator[Id],
  kmModule: KaplanMeierModule[Id]
)  
extends MTBResultSet
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

        val records =
          snps.map(_.data)

        val (therapyDistribution,meanTherapyDurations) =  
          therapyDistributionAndMeanDurations(records)

        Summary(
          id,
          records.size,
          ResultSet.Demographics.on(records.map(_.patient)),
          TumorDiagnostics(
            overallDiagnosticDistributions(records),
            diagnosticDistributionsByVariant(records)
          ),
          Medication(
            Medication.Recommendations(
              recommendationDistribution(records),
              recommendationsBySupportingVariant(records)
            ),
            Medication.Therapies(
              therapyDistribution,
              meanTherapyDurations,
              responsesByTherapy(records)  
            )
          ),
          kmModule.survivalReport(snps)
        )

    }

}
