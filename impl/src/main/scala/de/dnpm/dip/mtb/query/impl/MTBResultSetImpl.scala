package de.dnpm.dip.mtb.query.impl


import cats.{
  Applicative,
  Id
}
import de.dnpm.dip.coding.{
  CodeSystemProvider
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.{
  ICD10GM,
  ICDO3
}
import de.dnpm.dip.coding.icd.ClassKinds.Category
import de.dnpm.dip.model.Medications
import de.dnpm.dip.service.query.{
  PatientFilter,
  Query
}
import de.dnpm.dip.mtb.model.MTBPatientRecord
import de.dnpm.dip.mtb.query.api.{
  DiagnosisFilter,
  KaplanMeier,
  MTBQueryCriteria,
  MTBFilters,
  MTBResultSet,
  Ranked,
  RecommendationFilter,
  TherapyFilter
}


class MTBResultSetImpl
(
  val id: Query.Id,
  val queryCriteria: Option[MTBQueryCriteria],
  val results: Seq[Query.Match[MTBPatientRecord,MTBQueryCriteria]],
)(
  implicit
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

  import MTBResultSet._
  import Medications._
  import de.dnpm.dip.coding.icd.ICD.extensions._



  override implicit def toPredicate[F >: MTBFilters](
    mtbFilter: F
  ): MTBPatientRecord => Boolean = {

    import PatientFilter.Extensions._
    import MTBFilterExtensions._

    val MTBFilters(patientFilter,diagnosisFilter,recommendationFilter,therapyFilter) =
      mtbFilter.asInstanceOf[MTBFilters]

    record =>
      patientFilter(record.patient) &&
      diagnosisFilter(record.diagnoses.toList) &&
      recommendationFilter(record.getCarePlans.flatMap(_.medicationRecommendations.getOrElse(List.empty))) &&
      therapyFilter(record.getSystemicTherapies.map(_.latest))

  }


  override lazy val defaultFilter: MTBFilters = {

    val records = patientRecords(_ => true)

    MTBFilters(
      PatientFilter.on(records),
      DiagnosisFilter(
        Some(
          records
            .flatMap(_.diagnoses.map(_.code).toList)
            .toSet
            .pipe(
              // Add the parent category of each occurring entry,
              // so that e.g. [C25.1, C25.3, ...] -> [C25, C25.1, C25.3, ...]
              icd10s => icd10s ++ icd10s.flatMap(_.parentOfKind(Category))
            )
        )
      ),
      RecommendationFilter(
        Some(
          records.flatMap(_.getCarePlans)
            .flatMap(_.medicationRecommendations.getOrElse(List.empty))
            .map(_.medication)
            .toSet
            .pipe(
              // Add the medication class/group of each occurring entry,
              // so that medications e.g. [A.1, A.2, ...] -> [A, A.1, A.2, ...]
              meds => meds ++ meds.map(_.flatMap(_.currentGroup))
            )
        )
      ),
      TherapyFilter(
        Some(
          records.flatMap(_.getSystemicTherapies)
            .map(_.latest)
            .flatMap(_.medication)
            .toSet
            .pipe(
              // Add the medication class/group of each occurring entry,
              // so that medications e.g. [A.1, A.2, ...] -> [A, A.1, A.2, ...]
              meds => meds ++ meds.map(_.flatMap(_.currentGroup))
            )
        )
      )
    )
  }


  override def tumorDiagnostics(
    filter: MTBFilters
  ): TumorDiagnostics = {

    val records = patientRecords(filter)

    TumorDiagnostics(
      overallDiagnosticDistributions(records),
      diagnosticDistributionsByAlteration(records,queryCriteria.flatMap(_.geneAlterations))
    )
  }


  override def geneAlterations(
    filter: MTBFilters
  ): Seq[Ranked[MTBResultSet.GeneAlterationInfo]] =
    geneAlterationInfos(
      patientRecords(filter),
      queryCriteria
    )


  override def alteredGeneDistributions(
    filter: MTBFilters
  ) =
    alteredGeneDistributions(patientRecords(filter))


  override def medication(
    filter: MTBFilters
  ): MTBResultSet.Medication = {

    val records =
      patientRecords(filter)

    val (therapyDistribution,meanTherapyDurations) =  
      therapyDistributionAndMeanDurations(records)

    MTBResultSet.Medication(
      MTBResultSet.Medication.Recommendations(
        recommendationDistribution(records),
        recommendationsBySupportingAlteration(records,queryCriteria.flatMap(_.geneAlterations))
      ),
      MTBResultSet.Medication.Therapies(
        therapyDistribution,
        meanTherapyDurations
      )
    )
  }


  override def survivalStatistics(
    survivalType: Option[KaplanMeier.SurvivalType.Value],
    grouping: Option[KaplanMeier.Grouping.Value]
  )(
    implicit env: Applicative[Id]
  ) =
    kmModule.survivalStatistics(
      survivalType,
      grouping,
      results.map(_.record)
    )


  override def therapyResponses(
    filter: MTBFilters
  ): Seq[Ranked[TherapyResponses]] =
    therapyResponses(
      patientRecords(filter),
      queryCriteria
    )

}
