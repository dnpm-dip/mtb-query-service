package de.dnpm.dip.mtb.query.api


import java.time.{
  Instant,
  LocalDateTime
}
import cats.data.NonEmptyList
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.icd.{
  ICD10GM,
  ICDO3
}
import de.dnpm.dip.model.{
  Gender,
  Id,
  Interval,
  Medications,
  Site,
  Therapy,
  VitalStatus
}
import de.dnpm.dip.mtb.model.{
  ECOG,
  MTBTherapy
}
import de.dnpm.dip.service.Distribution
import de.dnpm.dip.service.query.{
  Query,
  Querier
}
import de.dnpm.dip.service.{
  ConnectionStatus,
  PeerToPeerRequest
}
import play.api.libs.json.{
  Json,
  OFormat,
  OWrites,
  Reads
}


case class FeasibilityQuery
(
  id: Id[FeasibilityQuery],
  submittedAt: LocalDateTime,
  querier: Querier,
  mode: Coding[Query.Mode.Value],
  criteria: Option[MTBQueryCriteria],
  peers: Seq[ConnectionStatus],
  expiresAfter: Int,
  lastUpdate: Instant
)


object FeasibilityQuery
{

  sealed trait Command
  case class Submit
  (
    mode: Coding[Query.Mode.Value],
    criteria: Option[MTBQueryCriteria]
  )
  extends Command

  case class Update
  (
    id: Id[FeasibilityQuery],
    mode: Option[Coding[Query.Mode.Value]],
    criteria: Option[MTBQueryCriteria]
  )
  extends Command

  case class Delete(id: Id[FeasibilityQuery]) extends Command

  sealed trait Error
  case object NoResults extends Error


  sealed trait Results
  {
    val cohortSize: Int
    val gender: Distribution[Coding[Gender.Value]]
    val age: Distribution[Interval[Int]]
    val vitalStatus: Distribution[Coding[VitalStatus.Value]]
    val tumorEntities: Distribution[Coding[ICD10GM]]
    val tumorMorphologies: Distribution[Coding[ICDO3]]
    val recommendedMedication: Distribution[Set[Coding[Medications]]]
    val therapyStatus: Distribution[Coding[Therapy.Status.Value]]
    val therapyStatusReason: Distribution[Coding[MTBTherapy.StatusReason.Value]]
    val ecogStatus: Distribution[Coding[ECOG.Value]]
    val usedMedication: Distribution[Set[Coding[Medications]]]
  }

  case class LocalResults
  (
    site: Coding[Site],
    cohortSize: Int,
    gender: Distribution[Coding[Gender.Value]],
    age: Distribution[Interval[Int]],
    vitalStatus: Distribution[Coding[VitalStatus.Value]],
    tumorEntities: Distribution[Coding[ICD10GM]],
    tumorMorphologies: Distribution[Coding[ICDO3]],
    recommendedMedication: Distribution[Set[Coding[Medications]]],
    therapyStatus: Distribution[Coding[Therapy.Status.Value]],
    therapyStatusReason: Distribution[Coding[MTBTherapy.StatusReason.Value]],
    ecogStatus: Distribution[Coding[ECOG.Value]],
    usedMedication: Distribution[Set[Coding[Medications]]]
  )
  extends Results

  case class AggregatedResults
  (
    query: Id[FeasibilityQuery],
    sites: NonEmptyList[Coding[Site]],
    cohortSize: Int,
    gender: Distribution[Coding[Gender.Value]],
    age: Distribution[Interval[Int]],
    vitalStatus: Distribution[Coding[VitalStatus.Value]],
    tumorEntities: Distribution[Coding[ICD10GM]],
    tumorMorphologies: Distribution[Coding[ICDO3]],
    recommendedMedication: Distribution[Set[Coding[Medications]]],
    therapyStatus: Distribution[Coding[Therapy.Status.Value]],
    therapyStatusReason: Distribution[Coding[MTBTherapy.StatusReason.Value]],
    ecogStatus: Distribution[Coding[ECOG.Value]],
    usedMedication: Distribution[Set[Coding[Medications]]]
  )
  extends Results


  case class Request
  (
    origin: Coding[Site],
    querier: Querier,
    criteria: Option[MTBQueryCriteria]
  )
  extends PeerToPeerRequest
  {
    type ResultType = LocalResults
  }


  trait Operations[F[_],Ctx]
  {

    def !(
      cmd: Command
    )(
      implicit ctx: Ctx
    ): F[Either[String,FeasibilityQuery]]


    def feasibilityQuery(
      id: Id[FeasibilityQuery]
    )(
      implicit ctx: Ctx
    ): F[Option[FeasibilityQuery]]


    def aggregatedResults(
      query: Id[FeasibilityQuery]
    )(
      implicit ctx: Ctx
    ): F[Option[AggregatedResults]]


    def process(
      req: Request
    )(
      implicit ctx: Ctx
    ): F[LocalResults]
  }


  import de.dnpm.dip.util.json.writesNel

  implicit val readsSubmit: Reads[Submit] =
    Json.reads[Submit]

  implicit val writesFeasibilityQuery: OWrites[FeasibilityQuery] =
    Json.writes[FeasibilityQuery]

  implicit val formatLocalResults: OFormat[LocalResults] =
    Json.format[LocalResults]

  implicit val writesAggregatedResults: OWrites[AggregatedResults] =
    Json.writes[AggregatedResults]
}
