package de.dnpm.dip.mtb.query.api


import de.dnpm.dip.model.UnitOfTime
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.service.query.{
  Entry,
  Query,
  Querier
}
import play.api.libs.json.{
  Json,
  OWrites,
  Writes,
  JsString
}

object KaplanMeier
{

  sealed trait SurvivalType
  {
    type Grouping
  }

  final case object OS extends SurvivalType
  {
    type Grouping = Coding[ICD10GM]
  }

  final case object PFS extends SurvivalType
  {
    type Grouping = Set[String]
  }

 
  implicit val writesOS: Writes[OS.type] =
    Writes { os => JsString("OS")}

  implicit val writesPFS: Writes[PFS.type] =
    Writes { pfs => JsString("PFS")}

  implicit val writesSurvivalType: Writes[SurvivalType] =
    Writes { 
      case OS  => Json.toJson(OS)
      case PFS => Json.toJson(PFS)
    }

/*
  implicit val writesSurvivalType: Writes[SurvivalType] =
    Writes { 
      case OS  => JsString("OS")
      case PFS => JsString("PFS")
    }
*/


  final case class CohortData
  (
    survivalRates: Seq[(Long,Double)],
    medianSurvivalTime: Long
  )


//  final case class SurvivalStatistics[T <: SurvivalType]
//    `type`: T,
//    timeUnit: UnitOfTime,
//    cohorts: Seq[Entry[Grouping,CohortData]]
//  )

  final case class SurvivalStatistics[Grouping]
  (
    timeUnit: UnitOfTime,
    cohorts: Seq[Entry[Grouping,CohortData]]
  )


  final case class CombinedSurvivalStatistics
  (
    os: SurvivalStatistics[OS.Grouping],
    pfs: SurvivalStatistics[PFS.Grouping]
  )

  implicit val writesCohortData: OWrites[CohortData] =
    Json.writes[CohortData]


  implicit def writesSurvivalStatistics[Grouping: Writes]: OWrites[SurvivalStatistics[Grouping]] =
    Json.writes[SurvivalStatistics[Grouping]]

  implicit def writesCombinedStatistics: OWrites[CombinedSurvivalStatistics] =
    Json.writes[CombinedSurvivalStatistics]


}



trait KaplanMeierOps[F[_],Env]
{

  def combinedSurvivalStatistics(
    query: Query.Id,
  )(
    implicit
    env: Env,
    user: Querier
  ): F[Option[KaplanMeier.CombinedSurvivalStatistics]]

/*
  def survivalStatistics[T <: KaplanMeier.SurvivalType](
    query: Query.Id,
    typ: T
  )(
    implicit env: Env
  ): F[Option[KaplanMeier.SurvivalStatistics[T]]]
*/

}
