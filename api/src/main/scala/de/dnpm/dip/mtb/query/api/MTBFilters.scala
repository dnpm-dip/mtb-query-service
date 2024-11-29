package de.dnpm.dip.mtb.query.api


import play.api.libs.json.{
  Json,
  OWrites
}
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.service.query.{
  Filters,
  PatientFilter
}
import de.dnpm.dip.model.Medications
import de.dnpm.dip.mtb.model.MTBPatientRecord



final case class DiagnosisFilter
(
  code: Option[Set[Coding[ICD10GM]]]
)

final case class RecommendationFilter
(
  medication: Option[Set[Set[Coding[Medications]]]]
)

final case class TherapyFilter
(
  medication: Option[Set[Set[Coding[Medications]]]]
)


final case class MTBFilters
(
  patient: PatientFilter,
  diagnosis: DiagnosisFilter,
  therapyRecommendation: RecommendationFilter,
  therapy: TherapyFilter
)
extends Filters[MTBPatientRecord]


object MTBFilters
{

  lazy val empty: MTBFilters =
    MTBFilters(
      PatientFilter.empty,
      DiagnosisFilter(None),
      RecommendationFilter(None),
      TherapyFilter(None)
    )


  implicit val writesDiagnosisFilter: OWrites[DiagnosisFilter] =
    Json.writes[DiagnosisFilter]

  implicit val writesRecommendationFilter: OWrites[RecommendationFilter] =
    Json.writes[RecommendationFilter]

  implicit val writesTherapyFilter: OWrites[TherapyFilter] =
    Json.writes[TherapyFilter]

  implicit val writes: OWrites[MTBFilters] =
    Json.writes[MTBFilters]

}
