package de.dnpm.dip.mtb.query.api

import cats.data.NonEmptyList
import de.dnpm.dip.model.{
  ClosedInterval,
  Medications,
  Patient,
  Reference,
  Site,
  UnitOfTime
}
import de.dnpm.dip.coding.{
  Coding,
  CodedEnum,
  DefaultCodeSystem
}
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.service.{
  ConnectionStatus,
  Count,
  Entry,
  PeerToPeerRequest
}
import play.api.libs.json.{
  Json,
  Format,
  OFormat,
  OWrites
}


object PFSRatio
{

  final case class DataPoint
  (
    patient: Reference[Patient],
    medication1: Set[Coding[Medications]],
    medication2: Set[Coding[Medications]],
    pfs1: Long,
    pfs2: Long,
    pfsRatio: Double
  )

  final case class CohortResult
  (
    dataPoints: Seq[DataPoint],
    median: Option[Double],
    partAboveThreshold: Count
  )

  final case class Report
  (
    timeUnit: UnitOfTime,
    cohorts: Seq[Entry[Coding[ICD10GM],CohortResult]] 
  )

  implicit val writesDataPoint: OWrites[DataPoint] =
    Json.writes[DataPoint]

  implicit val writesCohortResult: OWrites[CohortResult] =
    Json.writes[CohortResult]

  implicit val writesReport: OWrites[Report] =
    Json.writes[Report]

}


object KaplanMeier
{

  object SurvivalType
  extends CodedEnum("dnpm-dip/kaplan-meier-analysis/survival-type")
  with DefaultCodeSystem
  {
    val OS  = Value("os")
    val PFS = Value("pfs")

    override val display =
      Map(
        OS  -> "Overall Survival",
        PFS -> "Progression-free Survival"
      )

    implicit val format: Format[Value] =
      Json.formatEnum(this)
  }

  object Grouping
  extends CodedEnum("dnpm-dip/kaplan-meier-analysis/grouping")
  with DefaultCodeSystem
  {
    val Therapy         = Value("therapy")
    val TumorEntity     = Value("tumor-entity")
    val ObtainedTherapy = Value("treatment-obtained")
    val Ungrouped       = Value("none")

    override val display =
      Map(
        TumorEntity     -> "Tumor-Entität",
        Therapy         -> "Therapie",
        ObtainedTherapy -> "Therapie erhalten: ja/nein",
        Ungrouped       -> "Keine"
      )

    implicit val format: Format[Value] =
      Json.formatEnum(this)
  }


  final case class Config
  (
    entries: Seq[Entry[Coding[SurvivalType.Value],Seq[Coding[Grouping.Value]]]],
    defaults: Config.Defaults
  )

  object Config
  {
    final case class Defaults
    (
      `type`: SurvivalType.Value,
      grouping: Grouping.Value
    )

    implicit val writesDefaults: OWrites[Defaults] =
      Json.writes[Defaults]
    
    implicit val writesConfig: OWrites[Config] =
      Json.writes[Config]

  }

  final case class RawDataPoint
  (
    groupLabel: String,
    time: Long,
    event: Boolean
  )

  final case class RawSurvivalStatistics
  (
    site: Coding[Site],
    survivalType: Coding[SurvivalType.Value],
    grouping: Coding[Grouping.Value],
    timeUnit: UnitOfTime,
    data: Seq[RawDataPoint]
  )

  final case class RawSurvivalStatisticsRequest
  (  
    origin: Coding[Site] = Site.local,
    survivalTypeAndGrouping: Option[(SurvivalType.Value,Option[Grouping.Value])] = None,
    timeUnit: Option[UnitOfTime] = None
  )
  extends PeerToPeerRequest
  {
    type ResultType = RawSurvivalStatistics 
  }

  final case class GlobalSurvivalStatistics
  (
    survivalType: Coding[SurvivalType.Value],
    grouping: Coding[Grouping.Value],
    timeUnit: UnitOfTime,
    data: Seq[Entry[String,CohortResult]],
    peers: Seq[ConnectionStatus]
  )

  sealed trait Error
  final case object NoResults extends Error
  final case class ConnectionErrors(messages: NonEmptyList[String]) extends Error
  final case class GenericError(message: String) extends Error


  final case class DataPoint
  (
    time: Long,
    survRate: Double,
    censored: Boolean,
    confInterval: ClosedInterval[Double]
  )

  final case class CohortResult
  (
    survivalRates: Seq[DataPoint],
    medianSurvivalTime: Long
  )

  final case class SurvivalStatistics
  (
    survivalType: Coding[SurvivalType.Value],
    grouping: Coding[Grouping.Value],
    timeUnit: UnitOfTime,
    data: Seq[Entry[String,CohortResult]]
  )

  implicit val formatRawDataPoint: OFormat[RawDataPoint] =
    Json.format[RawDataPoint]

  implicit val formatRawSurvivalStatistics: OFormat[RawSurvivalStatistics] =
    Json.format[RawSurvivalStatistics]

  implicit val writeRawSurvivalStatisticsRequest: OWrites[RawSurvivalStatisticsRequest] =
    Json.writes[RawSurvivalStatisticsRequest]

  implicit val writesDataPoint: OWrites[DataPoint] =
    Json.writes[DataPoint]

  implicit val writesCohortResult: OWrites[CohortResult] =
    Json.writes[CohortResult]

  implicit val writesSurvivalStatistics: OWrites[SurvivalStatistics] =
    Json.writes[SurvivalStatistics]

  implicit val writeGlobalSurvivalStatistics: OWrites[GlobalSurvivalStatistics] =
    Json.writes[GlobalSurvivalStatistics]

}


import KaplanMeier._

trait GlobalKaplanMeierOps[F[_],Env]
{

  def !(
    request: RawSurvivalStatisticsRequest
  )(
    implicit env: Env
  ): F[Either[String,RawSurvivalStatistics]]


  def survivalStatistics(
    survivalTypeAndGrouping: Option[(SurvivalType.Value,Option[Grouping.Value])],
  )(
    implicit env: Env
  ): F[Either[Error,GlobalSurvivalStatistics]]

}

//TODO: remove once fully switched to globalSurvivalStatistics
trait KaplanMeierOps[F[_],Env]
{

  def survivalStatistics(
    survivalType: Option[SurvivalType.Value],
    grouping: Option[Grouping.Value]
  )(
    implicit env: Env
  ): F[SurvivalStatistics]

}
