package de.dnpm.dip.mtb.query.api


import java.time.LocalDate
import cats.data.IorNel
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.model.{
  Age,
  Gender,
  Interval,
  Period
}
import de.dnpm.dip.service.query.{
  ConceptCount,
  Entry
}
import play.api.libs.json.{
  Json,
  OWrites
}


final class MTBReportingCriteria
(
  period: Period[LocalDate]
)


object MTBReporting
{

  final case class AgeData
  (
    distribution: Seq[ConceptCount[Interval[Int]]],
    mean: Age,
    median: Age
  )

  final case class MedicationData
  (
    recommendations: Seq[ConceptCount[Set[String]]],
    recommendationsBySupportingVariant: Seq[Entry[String,Seq[ConceptCount[Set[String]]]]],
    therapies: Seq[ConceptCount[Set[String]]],
  )


  final case class Data
  (
    age: AgeData,
    tumorEntities: Seq[ConceptCount[Coding[ICD10GM]]],
    medication: MedicationData
  )



  implicit val formatAgeData: OWrites[AgeData] =
    Json.writes[AgeData]

  implicit val formatMedicationData: OWrites[MedicationData] =
    Json.writes[MedicationData]

  implicit val format: OWrites[Data] =
    Json.writes[Data]

}


trait MTBReporting[F[_],Env]
{

  def ?(criteria: MTBReportingCriteria)(
    implicit env: Env
  ): F[String IorNel MTBReporting.Data]

}
