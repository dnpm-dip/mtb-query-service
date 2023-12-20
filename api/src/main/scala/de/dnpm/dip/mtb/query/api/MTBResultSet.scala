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
  ConceptCount,
  Entry
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

  final case class TumorDiagnostics
  (
    tumorEntityDistribution: Seq[ConceptCount[Coding[ICD10GM]]],
    tumorMorphologyDistribution: Seq[ConceptCount[Coding[ICDO3.M]]]
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
      overallDistribution: Seq[ConceptCount[Set[String]]],
//    distributionBySupportingVariant: Seq[Entry[Reference[Variant],Seq[Entry[Set[String],Int]]]]
    )

    final case class Therapies
    (
      overallDistribution: Seq[Entry[Set[String],(Int,Duration)]],
//      responseDistributionsByTherapy: Seq[Entry[Set[String],Seq[ConceptCount[Coding[RECIST.Value]]]]]
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
    tumorDiagnostics: TumorDiagnostics,
    medication: Medication
  )
  extends ResultSet.Summary



  implicit val writesTumorDiagnostics: OWrites[TumorDiagnostics] =
    Json.writes[TumorDiagnostics]

  implicit val writesMedication: OWrites[Medication] =
    Json.writes[Medication]

  implicit val writes: OWrites[Summary] =
    Json.writes[Summary]

}
