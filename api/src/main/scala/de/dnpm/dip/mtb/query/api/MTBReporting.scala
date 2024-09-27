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
  Study,
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
}


final class MTBReportingCriteria
(
  period: Period[LocalDate]
)



import MTBReport._


final case class MTBReport
(
//  criteria: ...
  ageStats: AgeStats,
  demographics: ResultSet.Demographics,
  processSteps: Seq[Entry[Coding[ProcessStep.Value],Durations]],
  tumorEntities: Distribution[Coding[ICD10GM]],
  recommendations: Recommendations,
  claimResponses: ClaimResponses,
  followUp: FollowUp
)


object MTBReport
{

  object ProcessStep
  extends CodedEnum("dnpm-dip/mtb/report/process-steps")
  with DefaultCodeSystem
  {
    val ReferralToCarePlan = Value("referral-to-careplan")
    val ReferralToTherapy  = Value("referral-to-therapy")
    val CarePlanToTherapy  = Value("careplan-to-therapy")

    override val display =
      Map(
        ReferralToCarePlan -> "Anmeldung bis MTB-Beschluss",
        ReferralToTherapy  -> "Anmeldung bis Therapie-Beginn",
        CarePlanToTherapy  -> "MTB-Beschluss bis Therapie-Beginn"
      )

  }


  final case class Durations
  (
    mean: Duration,
    median: Duration
  )


  final case class AgeStats
  (
    distribution: Distribution[Interval[Int]],
    mean: Age,
    median: Age
  )


  final case class Recommendations
  (
    overallCount: Count,
    genders: Distribution[Coding[Gender.Value]],
    ages: Distribution[Interval[Int]],
    recommendations: MTBResultSet.Medication.Recommendations,
    evidenceLevels: Distribution[Coding[LevelOfEvidence.Grading.Value]],
    studyRecommendations: RecommendationData.StudyRecommendations
  )

  object RecommendationData
  {

    final case class StudyRecommendations
    (
      genders: Distribution[Coding[Gender.Value]],
      ages: Distribution[Interval[Int]],
      studies: Distribution[ExternalId[Study]]
    )

    implicit val formatStudyRecommendations: OWrites[StudyRecommendations] =
      Json.writes[StudyRecommendations]

  }


  final case class ClaimResponses
  (
    status: Coding[ClaimResponse.Status.Value],
    count: Count,
    statusReasons: Distribution[Coding[ClaimResponse.StatusReason.Value]]
  )

  final case class FollowUp
  (
    patientCount: Count,
    therapyData: Seq[Entry[Coding[Therapy.Status.Value],Distribution[Coding[Therapy.StatusReason]]]]
  )


  implicit val formatDurations: OWrites[Durations] =
    Json.writes[Durations]

  implicit val formatAgeStats: OWrites[AgeStats] =
    Json.writes[AgeStats]

  implicit val formatRecommendations: OWrites[Recommendations] =
    Json.writes[Recommendations]

  implicit val formatClaimResponses: OWrites[ClaimResponses] =
    Json.writes[ClaimResponses]

  implicit val formatFollowUp: OWrites[FollowUp] =
    Json.writes[FollowUp]

  implicit val format: OWrites[MTBReport] =
    Json.writes[MTBReport]

}


trait MTBReportOps[F[_],Env]
{

  def ?(criteria: MTBReportingCriteria)(
    implicit env: Env
  ): F[String IorNel MTBReport]

}
