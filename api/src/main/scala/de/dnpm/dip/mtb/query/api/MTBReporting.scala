package de.dnpm.dip.mtb.query.api


import java.time.LocalDate
import cats.data.IorNel
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.model.{
  Age,
  Duration,
  Gender,
  Interval,
  Period
}
import de.dnpm.dip.service.query.{
  ConceptCount,
  Distribution,
  Entry,
  ResultSet
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

  final case class Durations
  (
    mean: Duration,
    median: Duration
  )

  final case class AgeData
  (
    distribution: Distribution[Interval[Int]],
    mean: Age,
    median: Age
  )

  final case class MedicationData
  (
    recommendations: Distribution[Set[String]],
    recommendationsBySupportingVariant: Seq[Entry[String,Distribution[Set[String]]]],
    therapies: Distribution[Set[String]],
  )


  final case class Report
  (
    ageData: AgeData,
    demographics: ResultSet.Demographics,
    tumorEntities: Distribution[Coding[ICD10GM]],
    medication: MedicationData
  )



  implicit val formatAgeData: OWrites[AgeData] =
    Json.writes[AgeData]

  implicit val formatMedicationData: OWrites[MedicationData] =
    Json.writes[MedicationData]

  implicit val formatReport: OWrites[Report] =
    Json.writes[Report]

}


trait MTBReporting[F[_],Env]
{

  def ?(criteria: MTBReportingCriteria)(
    implicit env: Env
  ): F[String IorNel MTBReporting.Report]

}
