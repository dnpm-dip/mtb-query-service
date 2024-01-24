package de.dnpm.dip.mtb.query.api


import de.dnpm.dip.model.{
  UnitOfTime,
  Interval
}
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.service.query.{
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


object KaplanMeier
{

  object SurvivalType extends Enumeration
  {
    val OS, PFS = Value

    implicit val format: Format[Value] =
      Json.formatEnum(this)
  }

  object Grouping extends Enumeration
  {
    val ByTherapy     = Value
    val ByTumorEntity = Value
    val Ungrouped     = Value

    implicit val format: Format[Value] =
      Json.formatEnum(this)
  }


  final case class DataPoint
  (
    time: Long,
    survRate: Double,
    censored: Boolean,
    confRange95: Interval[Double]
  )


  final case class CohortResult
  (
    survivalRates: Seq[DataPoint],
    medianSurvivalTime: Long
  )

  final case class SurvivalStatistics
  (
    survivalType: SurvivalType.Value,
    grouping: Grouping.Value,
    timeUnit: UnitOfTime,
    data: Seq[Entry[String,CohortResult]]
  )


  final case class SurvivalReport
  (
    survivalData: Seq[SurvivalStatistics]
  )



  implicit val writesDataPoint: OWrites[DataPoint] =
    Json.writes[DataPoint]

  implicit val writesCohortResult: OWrites[CohortResult] =
    Json.writes[CohortResult]

  implicit val writesSurvivalStatistics: OWrites[SurvivalStatistics] =
    Json.writes[SurvivalStatistics]

  implicit def writesStatisticsReport: OWrites[SurvivalReport] =
    Json.writes[SurvivalReport]

}



trait KaplanMeierOps[F[_],Env]
{

  def survivalReport(
    query: Query.Id,
  )(
    implicit
    env: Env,
    user: Querier
  ): F[Option[KaplanMeier.SurvivalReport]]

/*
  def survivalStatistics[T <: KaplanMeier.SurvivalType](
    query: Query.Id,
    typ: T
  )(
    implicit env: Env
  ): F[Option[KaplanMeier.SurvivalStatistics[T]]]
*/

}
