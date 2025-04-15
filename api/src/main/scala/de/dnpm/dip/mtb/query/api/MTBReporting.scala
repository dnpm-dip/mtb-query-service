package de.dnpm.dip.mtb.query.api


import java.time.LocalDate
import cats.data.IorNel
import play.api.libs.json.{
  Json,
  OFormat,
  OWrites
}
import de.dnpm.dip.coding.{
  Coding,
  CodedEnum,
  DefaultCodeSystem
}
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.util.DisplayLabel
import de.dnpm.dip.model.{
  Age,
  Duration,
  ExternalId,
  Gender,
  Interval,
  Medications,
  NGSReport,
  Period,
  Site,
  Study,
  Therapy
}
import de.dnpm.dip.service.query.{
  Count,
  Distribution,
  Entry
}
import de.dnpm.dip.mtb.model.{
  ClaimResponse,
  LevelOfEvidence,
  MTBTherapy,
  Variant
}


final case class MTBReportingCriteria
(
  period: Period[LocalDate],
  sites: Set[Coding[Site]]
)

object MTBReportingCriteria
{
  implicit val format: OFormat[MTBReportingCriteria] =
    Json.format[MTBReportingCriteria]
}


import MTBReport._


final case class MTBReport
(
  criteria: MTBReportingCriteria,
  demographics: Demographics,    
  tumorEntities: Distribution[Coding[ICD10GM]],
  processStepDurations: ProcessStepDurations,
  recommendations: Recommendations,
  claimResponses: Seq[Entry[Coding[ClaimResponse.Status.Value],ClaimResponses]],
  followUp: FollowUp,
  sequencingTypes: Distribution[Coding[NGSReport.Type.Value]]
)


object MTBReport
{

  object ProcessStep
  extends CodedEnum("dnpm-dip/mtb/report/process-steps")
  with DefaultCodeSystem
  {
    val ReferralToCarePlan = Value("referral-to-careplan")
//    val ReferralToTherapy  = Value("referral-to-therapy")
    val CarePlanToTherapy  = Value("careplan-to-therapy")

    override val display =
      Map(
        ReferralToCarePlan -> "Anmeldung bis MTB-Beschluss",
//        ReferralToTherapy  -> "Anmeldung bis Therapie-Beginn",
        CarePlanToTherapy  -> "MTB-Beschluss bis Therapie-Beginn"
      )

  }


  final case class Durations
  (
    mean: Duration,
    median: Duration
  )

  type ProcessStepDurations = Seq[Entry[Coding[ProcessStep.Value],Durations]]


  final case class Demographics
  (
    genderDistribution: Distribution[Coding[Gender.Value]],
    ageDistribution: Distribution[Interval[Int]],
    meanAge: Option[Age],
    medianAge: Option[Age]
  )


  final case class Recommendations
  (
    patientCount: Count,
    demographics: Demographics,    
    tumorEntities: Distribution[Coding[ICD10GM]],
    medicationRecommendations: Distribution[Set[Coding[Medications]]],
    medicationRecommendationsBySupportingVariant: Seq[Entry[DisplayLabel[Variant],Distribution[Set[Coding[Medications]]]]],
    evidenceLevels: Distribution[Coding[LevelOfEvidence.Grading.Value]],
    studyRecommendations: Recommendations.StudyRecommendations
  )

  object Recommendations
  {

    final case class StudyRecommendations
    (
      demographics: Demographics,    
      studies: Distribution[ExternalId[Study,Study.Registries]]
    )

    implicit val formatStudyRecommendations: OWrites[StudyRecommendations] =
      Json.writes[StudyRecommendations]

  }

  final case class ClaimResponses
  (
    count: Count,
    evidenceLevels: Distribution[Coding[LevelOfEvidence.Grading.Value]]
  )


  final case class FollowUp
  (
    livePatientCount: Count,
    therapyData: Seq[Entry[Coding[Therapy.Status.Value],Distribution[Coding[MTBTherapy.StatusReason.Value]]]],
    pfsRatio: PFSRatio.Report
  )


  implicit val formatDurations: OWrites[Durations] =
    Json.writes[Durations]

  implicit val formatDemographics: OWrites[Demographics] =
    Json.writes[Demographics]

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
