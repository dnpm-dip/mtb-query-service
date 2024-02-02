package de.dnpm.dip.mtb.query.api


import java.time.LocalDate
import cats.data.IorNel
import play.api.libs.json.{
  Json,
  OWrites
}
import de.dnpm.dip.coding.{
  Coding,
  CodedEnum,
  DefaultCodeSystem
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.model.{
  Age,
  Duration,
  ExternalId,
  Gender,
  Interval,
  Period,
  Therapy
}
import de.dnpm.dip.service.query.{
  Count,
  ConceptCount,
  Distribution,
  Entry,
  ResultSet
}
import de.dnpm.dip.mtb.model.{
  ClaimResponse,
  LevelOfEvidence,
  Study
}


final class MTBReportingCriteria
(
  period: Period[LocalDate]
)



import MTBReport._


final case class MTBReport
(
//  criteria: ...
  ageData: AgeData,
  demographics: ResultSet.Demographics,
  processStepDurations: Seq[Entry[Coding[ProcessStep.Value],Durations]],
  tumorEntities: Distribution[Coding[ICD10GM]],
  recommendationData: RecommendationData
)


object MTBReport
{

  object ProcessStep
  extends CodedEnum("dnpm-dip/mtb/report/process-steps")
  with DefaultCodeSystem
  {
    val ReferralToCarePlan = Value("referral-to-careplan")
    val ReferralToTherapy  = Value("referral-to-therapy")
//    val CarePlanToTherapy  = Value(careplan-to-therapy)

    override val display =
      Map(
        ReferralToCarePlan -> "Anmeldung bis MTB-Beschluss",
        ReferralToTherapy  -> "Anmeldung bis Therapie-Beginn"
//        CarePlanToTherapy  -> "MTB-Beschluss bis Therapie-Beginn"
      )

  }



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

  final case class RecommendationData
  (
    overallCount: Count,
    genderDistribution: Distribution[Coding[Gender.Value]],
    ageDistribution: Distribution[Interval[Int]],
    recommendations: MTBResultSet.Medication.Recommendations,
    evidenceLevels: Distribution[Coding[LevelOfEvidence.Grading.Value]],
    studyRecommendationData: RecommendationData.StudyRecommendationData
  )

  object RecommendationData
  {

    final case class StudyRecommendationData
    (
      genderDistribution: Distribution[Coding[Gender.Value]],
      ageDistribution: Distribution[Interval[Int]],
      studyDistribution: Distribution[ExternalId[Study]]
    )

    implicit val formatStudyRecommendationData: OWrites[StudyRecommendationData] =
      Json.writes[StudyRecommendationData]

  }


  final case class ClaimResponseData
  (
    status: Coding[ClaimResponse.Status.Value],
    count: Count,
    reasonDistribution: Distribution[Coding[ClaimResponse.StatusReason.Value]]
  )

  final case class FollowUpData
  (
    patientCount: Int,
    therapyData: Seq[Entry[Coding[Therapy.Status.Value],Distribution[Coding[Therapy.StatusReason]]]]

  )


  implicit val formatDurations: OWrites[Durations] =
    Json.writes[Durations]

  implicit val formatAgeData: OWrites[AgeData] =
    Json.writes[AgeData]

  implicit val formatRecommendationData: OWrites[RecommendationData] =
    Json.writes[RecommendationData]

  implicit val format: OWrites[MTBReport] =
    Json.writes[MTBReport]

}


trait MTBReportOps[F[_],Env]
{

  def ?(criteria: MTBReportingCriteria)(
    implicit env: Env
  ): F[String IorNel MTBReport]

}
