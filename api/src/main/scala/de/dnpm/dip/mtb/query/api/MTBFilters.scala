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
extends (MTBDiagnosis => Boolean)
{
  def apply(d: MTBDiagnosis) =
    code.exists(_.exists(_.code == d.code.code))
}


final case class MTBFilters
(
  patientFilter: PatientFilter,
//  diagnosisFilter: DiagnosisFilter
)
extends Filters[MTBPatientRecord]
{

  override def apply(patRec: MTBPatientRecord): Boolean = {

    patientFilter(patRec.patient) //&& patRec.diagnoses.exists(diagnosisFilter)

  }

}


object MTBFilters
{

  implicit val writesDiagnosisFilter: OWrites[DiagnosisFilter] =
    Json.writes[DiagnosisFilter]

  implicit val writes: OWrites[MTBFilters] =
    Json.writes[MTBFilters]

}
