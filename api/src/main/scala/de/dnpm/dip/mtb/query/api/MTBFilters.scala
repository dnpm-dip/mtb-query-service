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
import de.dnpm.dip.mtb.model.{
  MTBDiagnosis,
  MTBPatientRecord
}


final case class DiagnosisFilter
(
  code: Option[Set[Coding[ICD10GM]]]
)

final case class MTBFilters
(
  patientFilter: PatientFilter,
  diagnosisFilter: DiagnosisFilter
)
extends Filters[MTBPatientRecord]

object MTBFilters
{

  lazy val empty: MTBFilters =
    MTBFilters(
      PatientFilter.empty,
      DiagnosisFilter(None)
    )


  implicit val writesDiagnosisFilter: OWrites[DiagnosisFilter] =
    Json.writes[DiagnosisFilter]

  implicit val writes: OWrites[MTBFilters] =
    Json.writes[MTBFilters]

}
