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
import de.dnpm.dip.mtb.model.MTBPatientRecord


/*
final case class DiagnosisFilter
(
  code: Option[Seq[Coding[ICD10GM]]]
)
extends (MTBDiagnosis => Boolean)
*/


final case class MTBFilters
(
  patientFilter: PatientFilter
)
extends Filters[MTBPatientRecord]
{

  override def apply(patRec: MTBPatientRecord): Boolean = {

    patientFilter(patRec.patient) 
  }

}


object MTBFilters
{

  implicit val writes: OWrites[MTBFilters] =
    Json.writes[MTBFilters]

}
