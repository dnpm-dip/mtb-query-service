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
  Quantity
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
    entities: Seq[ConceptCount[Coding[ICD10GM]]],
    morphologies: Seq[ConceptCount[Coding[ICDO3.M]]]
  )


  final case class Medication
  (
    recommendationsTotal: Seq[ConceptCount[Coding[ATC]]],
    recommendationsBySupportingVariant: Seq[Entry[Reference[Variant],Seq[ConceptCount[Coding[ATC]]]]],
    therapies: Seq[ConceptCount[Set[Coding[ATC]]]],
    meanTherapyDurations: Seq[Entry[Set[Coding[ATC]],Int]],
    responsesByTherapy: Seq[Entry[Set[Coding[ATC]],Seq[ConceptCount[Coding[RECIST.Value]]]]]
  )

  final case class Summary
  (
    id: Query.Id,
    numPatients: Int,
    demographics: ResultSet.Demographics,
    diagnostics: TumorDiagnostics,
//    medication: Medication
  )
  extends ResultSet.Summary



  implicit val writesTumorDiagnostics: OWrites[TumorDiagnostics] =
    Json.writes[TumorDiagnostics]

  implicit val writesMedication: OWrites[Medication] =
    Json.writes[Medication]

  implicit val writes: OWrites[Summary] =
    Json.writes[Summary]

}
