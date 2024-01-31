package de.dnpm.dip.mtb.query.api


import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.coding.icd.ICDO3
import de.dnpm.dip.coding.hgvs.HGVS
import de.dnpm.dip.model.{
  Gender,
  Interval,
  Site,
  Reference,
  Duration
}
import de.dnpm.dip.service.query.{
  PatientFilter,
  Query,
  ResultSet,
  Entry,
  Distribution
}
import de.dnpm.dip.mtb.model.{
  MTBPatientRecord,
  Variant,
  RECIST
}
import play.api.libs.json.{
  Json,
  OWrites
}


trait MTBResultSet
extends ResultSet[MTBPatientRecord,MTBQueryCriteria]
{
  type SummaryType = MTBResultSet.Summary

}


object MTBResultSet
{

  object TumorDiagnostics
  { 

    final case class Distributions
    (
      tumorEntities: Distribution[Coding[ICD10GM]],
      tumorMorphologies: Distribution[Coding[ICDO3.M]]
    )

    implicit val writesDistributions: OWrites[Distributions] =
      Json.writes[Distributions]

  }

  final case class TumorDiagnostics
  (
    overallDistributions: TumorDiagnostics.Distributions,
    distributionsByVariant: Seq[Entry[String,TumorDiagnostics.Distributions]]
  )


  final case class Medication
  (
    recommendations: Medication.Recommendations,
    therapies: Medication.Therapies
  )
  object Medication
  {

    final case class Recommendations
    (
      overallDistribution: Distribution[Set[String]],
      distributionBySupportingVariant: Seq[Entry[String,Distribution[Set[String]]]]
    )

    final case class Therapies
    (
      overallDistribution: Distribution[Set[String]],
      meanDurations: Seq[Entry[Set[String],Double]],
      responseDistributionByTherapy: Seq[Entry[Set[String],Distribution[Coding[RECIST.Value]]]]
    )


    implicit val writesRecommendations: OWrites[Recommendations] =
      Json.writes[Recommendations]

    implicit val writesTherapies: OWrites[Therapies] =
      Json.writes[Therapies]

    implicit val writes: OWrites[Medication] =
      Json.writes[Medication]

  }
 


  final case class Summary
  (
    id: Query.Id,
    patientCount: Int,
    demographics: ResultSet.Demographics,
    diagnostics: TumorDiagnostics,
    medication: Medication,
    survivalReport: KaplanMeier.SurvivalReport
  )
  extends ResultSet.Summary



  implicit val writesTumorDiagnostics: OWrites[TumorDiagnostics] =
    Json.writes[TumorDiagnostics]

  implicit val writesMedication: OWrites[Medication] =
    Json.writes[Medication]

  implicit val writes: OWrites[Summary] =
    Json.writes[Summary]

}
