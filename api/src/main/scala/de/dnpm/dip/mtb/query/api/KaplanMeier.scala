package de.dnpm.dip.mtb.query.api


import de.dnpm.dip.model.{
  UnitOfTime,
  ClosedInterval
}
import de.dnpm.dip.coding.{
  Coding,
  CodedEnum,
  DefaultCodeSystem
}
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.service.query.{
  Count,
  Entry,
  Query,
  Querier
}
import play.api.libs.json.{
  Json,
  Format,
  OWrites,
  Writes,
  JsString
}


object PFSRatio
{

  final case class DataPoint
  (
    patient: String,
    pfs1: Long,
    pfs2: Long,
    pfsr: Double
  )

  final case class CohortResult
  (
    timeUnit: UnitOfTime,
    data: Seq[DataPoint],
    medianPfsr: Double,
    upperSubset: Count
  )

  type Report = Seq[Entry[String,CohortResult]] 

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
    val ByTherapy     = Value("by-therapy")
    val ByTumorEntity = Value("by-tumor-entity")
    val Ungrouped     = Value("none")

    override val display =
      Map(
        ByTherapy     -> "Nach Therapie",
        ByTumorEntity -> "Nach Tumor-Entität",
        Ungrouped     -> "Keine"
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


  implicit val writesDataPoint: OWrites[DataPoint] =
    Json.writes[DataPoint]

  implicit val writesCohortResult: OWrites[CohortResult] =
    Json.writes[CohortResult]

  implicit val writesSurvivalStatistics: OWrites[SurvivalStatistics] =
    Json.writes[SurvivalStatistics]

}


import KaplanMeier._

trait KaplanMeierOps[F[_],Env]
{

  def survivalStatistics(
    survivalType: Option[SurvivalType.Value],
    grouping: Option[Grouping.Value]
  )(
    implicit env: Env
  ): F[SurvivalStatistics]

}
