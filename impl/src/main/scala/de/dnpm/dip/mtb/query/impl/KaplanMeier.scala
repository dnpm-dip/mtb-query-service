package de.dnpm.dip.mtb.query.impl


import scala.util.chaining._
import java.time.{
  Instant,
  LocalDate,
  ZoneId
}
import java.time.temporal.ChronoUnit
import cats.{
  Applicative,
  Monad,
}
import de.dnpm.dip.coding.{
  Coding,
  CodeSystemProvider
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.{
  ICD,
  ICD10GM
}
import de.dnpm.dip.coding.icd.ClassKinds.Category
import de.dnpm.dip.service.{
  Count,
  Entry
}
import de.dnpm.dip.service.query.ReportingOps
import de.dnpm.dip.model.{
  ClosedInterval,
  FollowUp,
  Id,
  Reference,
  Snapshot,
  UnitOfTime
}
import de.dnpm.dip.model.Medications._
import de.dnpm.dip.mtb.model.{
  ECOG,
  MTBDiagnosis,
  MTBPatientRecord,
  MTBTherapy,
  MTBSystemicTherapy,
  Response,
  RECIST
}
import MTBTherapy.StatusReason.Progression
import de.dnpm.dip.mtb.query.api.PFSRatio
import de.dnpm.dip.mtb.query.api.KaplanMeier.{
  Config,
  SurvivalType,
  Grouping,
  DataPoint,
  CohortResult,
  SurvivalStatistics
}
import SurvivalType._
import Grouping._



trait SurvivalOps
{ 

  // Inclusion of SD as marker of 'progression' was specified by domain experts
  protected val progression = Set(RECIST.PD,RECIST.SD).map(Coding(_))

  val dateOfDeathOrCensoring: Snapshot[MTBPatientRecord] => (LocalDate,Boolean) = {

    case Snapshot(record,t) =>
    
      record.patient.dateOfDeath
        // Else check if an ECOG 5 (death) occurs
        .orElse(
          record.performanceStatus.flatMap(
            _.collectFirst {
              case ecog if ecog.value.code.enumValue == ECOG.Five => ecog.effectiveDate
            } 
          )
        )
        // Else if last Follow-Up documents "Lost to follow-up", use FU date as event occurred
        .orElse(
          record.followUps.flatMap(
            _.maxByOption(_.date)
             .collect { 
               case followUp if followUp.patientStatus.exists(_.code.enumValue == FollowUp.PatientStatus.LostToFU) => followUp.date
             }
          )
        )
        .map(_ -> true)
        .getOrElse(
          // 1. Censoring time strategy: fall back to date of last therapy follow-up
          record
            .getSystemicTherapies
            .flatMap(_.history.map(_.recordedOn).toList)
            .maxOption
            // 2. Censoring time strategy: fall back to upload date
            .getOrElse(LocalDate.ofInstant(Instant.ofEpochMilli(t),ZoneId.systemDefault)) -> false
          )

  }


  def progressionOrCensoringDate(
    therapy: MTBSystemicTherapy,
    record: MTBPatientRecord,
  )(
    implicit lastResponses: Map[Id[MTBSystemicTherapy],Response]
  ): (LocalDate,Boolean) =
    lastResponses
      .get(therapy.id)
      // 1. Look for date of latest response with recorded progression
      .collect {
        case response if progression(response.value) => response.effectiveDate
      }
      // 2. Check whether therapy was stopped due to progression and take the end or recording date
      .orElse(
        therapy.statusReason.collect { 
          case MTBTherapy.StatusReason(Progression) => therapy.period.flatMap(_.endOption).getOrElse(therapy.recordedOn)
        }
      )
      // 3. Use patient date of death as "progression" date
      .orElse(record.patient.dateOfDeath)
      .orElse {

        val mtbTherapy = record.getSystemicTherapies.exists(_.history.exists(_.id == therapy.id))

        // 4. MTB Therapy: If last Follow-Up documents "Lost to follow-up", use FU date as event occurred
        if (mtbTherapy)
          record.followUps.flatMap(
            _.maxByOption(_.date)
             .collect { 
               case followUp if followUp.patientStatus.exists(_.code.enumValue == FollowUp.PatientStatus.LostToFU) => followUp.date
             }
          )
        // 5. Prior/guideline therapy: Use therapy end date as event occurred
        else therapy.period.flatMap(_.endOption)
      }
      .map(_ -> true)
      // 5. Censoring: therapy recording date
      .getOrElse(therapy.recordedOn -> false)
 
 
  def overallSurvival(
    diagnosis: MTBDiagnosis,
    snp: Snapshot[MTBPatientRecord]
  )(
    implicit chronoUnit: ChronoUnit
  ): Option[(Long,Boolean)] = {

    val (observationDate,status) = dateOfDeathOrCensoring(snp)

    Option(chronoUnit.between(diagnosis.recordedOn,observationDate))
      .collect { 
        case l if l > 0 => l -> status
      }
  }

  
  def progressionFreeSurvival(
    therapy: MTBSystemicTherapy,
    record: MTBPatientRecord
  )(
    implicit chronoUnit: ChronoUnit
  ): Option[(Long,Boolean)] = {

    implicit val lastResponses =
      record
        .getResponses
        .groupBy(_.therapy)
        .collect { 
          case (ref,responses) => ref.id -> responses.maxBy(_.effectiveDate)
        }

    val (observationDate,status) = progressionOrCensoringDate(therapy,record)

    therapy.period.map(_.start)
      .map(chronoUnit.between(_,observationDate))
      .collect { 
        case l if l > 0 => l -> status
      }

  }


  def pfsRatio(
    record: MTBPatientRecord
  )(
    implicit chronoUnit: ChronoUnit
  ): Option[(Coding[ICD10GM],PFSRatio.DataPoint)] = {

    implicit val diagnoses = record.diagnoses

    for {
      th1 <- record.getGuidelineTherapies.maxByOption(_.recordedOn)

      pfs1 <- progressionFreeSurvival(th1,record).map(_._1)

      medication1 <- th1.medication

      th2 <- record.getSystemicTherapies.map(_.latest).maxByOption(_.recordedOn)

      pfs2 <- progressionFreeSurvival(th2,record).map(_._1)

      medication2 <- th2.medication

      tumorEntity <-
        for {
          entity2 <- th2.reason.flatMap(_.resolve).map(_.code)
          entity1 <- th1.reason.flatMap(_.resolve).map(_.code)
          if entity1 == entity2
        } yield entity2

    } yield (
      tumorEntity,
      PFSRatio.DataPoint(
        Reference.to(record.patient),
        medication1,
        medication2,
        pfs1,
        pfs2,
        pfs2.toDouble/pfs1
      )
    )
          
  }


  def pfsRatio(
    mtbTherapy: MTBSystemicTherapy
  )(
    implicit
    record: MTBPatientRecord,
    chronoUnit: ChronoUnit
  ): Option[PFSRatio.DataPoint] = {
    for {
      th1 <- record.getGuidelineTherapies.maxByOption(_.recordedOn)

      pfs1 <- progressionFreeSurvival(th1,record).map(_._1)

      medication1 <- th1.medication

      pfs2 <- progressionFreeSurvival(mtbTherapy,record).map(_._1)

      medication2 <- mtbTherapy.medication

    } yield PFSRatio.DataPoint(
      Reference.to(record.patient),
      medication1,
      medication2,
      pfs1,
      pfs2,
      pfs2.toDouble/pfs1
    )
          
  }

  val responderThreshold = 1.3

}


trait KaplanMeierEstimator[F[_]]
{
  self =>

  def apply(
    input: Seq[(Long,Boolean)]
  )(
    implicit F: Monad[F]
  ): F[Seq[DataPoint]]


  def cohortResult(
    input: Seq[(Long,Boolean)]
  )(
    implicit F: Monad[F]
  ): F[CohortResult] = {

    import cats.syntax.functor._

    // Median survival time defined as: min{ t | Surv(t) <= 0.5 }
    def medianSt(ts: Seq[DataPoint]): Long =
      ts.collectFirst { case DataPoint(t,surv,_,_) if surv <= 0.5 => t }
        .getOrElse(0L)


    for {
      surv <- self(input)
    } yield CohortResult(
      surv,
      medianSt(surv)
    )

  }

}


trait KaplanMeierModule[F[_]] extends SurvivalOps
{
  self =>

  implicit val atc: CodeSystemProvider[ATC,cats.Id,Applicative[cats.Id]]

  implicit val icd10gm: CodeSystemProvider[ICD10GM,cats.Id,Applicative[cats.Id]]


  def survivalConfig: Config


  def survivalStatistics(
    survivalType: Option[SurvivalType.Value],
    grouping: Option[Grouping.Value],
    cohort: Seq[Snapshot[MTBPatientRecord]],
    timeUnit: UnitOfTime = UnitOfTime.Days
  )(
    implicit estimator: KaplanMeierEstimator[F],
  ): F[SurvivalStatistics]


  def pfsRatioReport(
    cohort: Seq[Snapshot[MTBPatientRecord]],
    timeUnit: UnitOfTime = UnitOfTime.Days
  )(
    implicit F: Monad[F]
  ): F[PFSRatio.Report]

}


class DefaultKaplanMeierModule(
  override implicit val atc: CodeSystemProvider[ATC,cats.Id,Applicative[cats.Id]],
  override implicit val icd10gm: CodeSystemProvider[ICD10GM,cats.Id,Applicative[cats.Id]]
)
extends KaplanMeierModule[cats.Id]
{

  import ICD.extensions._

  private val defaults = Config.Defaults(OS,ObtainedTherapy)

  override val survivalConfig: Config =
    Config(
      Seq(
        Entry(
          Coding(OS),
          Seq(Ungrouped,TumorEntity,ObtainedTherapy).map(Coding(_))
        ),
        Entry(
          Coding(PFS),
          Seq(Therapy).map(Coding(_))
        )
      ),
      defaults  
    )


  override def survivalStatistics(
    optSurvivalType: Option[SurvivalType.Value],
    optGrouping: Option[Grouping.Value],
    cohort: Seq[Snapshot[MTBPatientRecord]],
    timeUnit: UnitOfTime
  )(
    implicit estimator: KaplanMeierEstimator[cats.Id]
  ): SurvivalStatistics = {

    implicit val chronoUnit = UnitOfTime.chronoUnit(timeUnit)

    val survivalType = optSurvivalType.getOrElse(defaults.`type`)
    val grouping     = optGrouping.getOrElse(defaults.grouping)

    cohort
      .flatMap(projector(survivalType,grouping))
      .groupMap(_._1){
        case (_,duration,status) => duration -> status
      }
      .map {
        case (group,data) => Entry(group,estimator.cohortResult(data))
      }
      .toSeq
      .sortBy(_.key)
      .pipe(
        SurvivalStatistics(
          Coding(survivalType),
          Coding(grouping),
          timeUnit,
          _
        )
      )

  }


  private def projector(
    survivalType: SurvivalType.Value,
    grouping: Grouping.Value
  )(
    implicit chronoUnit: ChronoUnit
  ): Snapshot[MTBPatientRecord] => Iterable[(String,Long,Boolean)] =
    (survivalType,grouping) match {   

      case (OS,TumorEntity) =>
        snp => snp.data.diagnoses.toList.flatMap {
          diagnosis =>
            for {
              (os,status) <- overallSurvival(diagnosis,snp)
            } yield (
             // ICD-10 Category as group label
              diagnosis.code.parentOfKind(Category).getOrElse(diagnosis.code).code.value,
              os,
              status
            )
        }
      
      case (OS,ObtainedTherapy) =>

        def hasObtainedTherapy(record: MTBPatientRecord): Boolean =
          record.systemicTherapies.exists(_.exists { 
            history =>
              val latest = history.latestBy(_.recordedOn)
              latest.period.isDefined && latest.medication.exists(_.nonEmpty)
            }
          )

        snp => snp.data.diagnoses.toList.flatMap {
          diagnosis => 
            for {
              (os,status) <- overallSurvival(diagnosis,snp)
              group = if (hasObtainedTherapy(snp.data)) "Therapie erhalten" else "Keine Therapie erhalten"
            } yield (group,os,status)
        }

      case (OS,Ungrouped) =>
        snp =>
          snp.data.diagnoses.toList.minByOption(_.recordedOn).flatMap {
            diagnosis =>
              for {  
                (os,status) <- overallSurvival(diagnosis,snp)
              } yield ("Alle",os,status)
          }

      case (PFS,Therapy) => {
        case Snapshot(record,_) =>
          record.getSystemicTherapies.map(_.latest).flatMap {
            therapy =>
              for { 
                (pfs,status) <- progressionFreeSurvival(therapy,record)
          
                medClasses <-
                  therapy
                    .medication
                    .map(_.flatMap(_.currentGroup))
                    .map(_.flatMap(_.display))
          
              } yield (
                medClasses.mkString(" + "),
                pfs,
                status
              )
          }
      }

      case (PFS,Ungrouped) => {
        case Snapshot(record,_) =>
          record.getSystemicTherapies.map(_.latest).flatMap {
            therapy =>
              progressionFreeSurvival(therapy,record)
                .map {
                  case (pfs,status) => ("Alle",pfs,status)
                }
          }
      }

    }


  override def pfsRatioReport(
    cohort: Seq[Snapshot[MTBPatientRecord]],
    timeUnit: UnitOfTime
  )(
    implicit F: Monad[cats.Id]
  ): PFSRatio.Report = {

    implicit val chronoUnit = UnitOfTime.chronoUnit(timeUnit)

    PFSRatio.Report(
      timeUnit,
      cohort.flatMap(snp => pfsRatio(snp.data))
        .groupMap(_._1)(_._2)
        .map {
          case (entity,seq) => Entry(
            entity,
            seq.pipe {
              dataPoints =>
                PFSRatio.CohortResult(
                  dataPoints.zipWithIndex
                    .map {
                      case (pt,idx) => pt.copy(patient = pt.patient.withDisplay(s"Patient $idx"))
                    },
                  ReportingOps.median(dataPoints.map(_.pfsRatio)),
                  Count.of(
                    n     = dataPoints.count(_.pfsRatio >= responderThreshold),
                    total = dataPoints.size
                  )
                )
            }
          )
        }
        .toSeq
    )

  }

}



object DefaultKaplanMeierEstimator extends KaplanMeierEstimator[cats.Id]
{

  import scala.math.sqrt


  private val z = 1.96 // z-Factor for 95% confidence interval


  override def apply(
    input: Seq[(Long,Boolean)]
  )(
    implicit F: Monad[cats.Id]
  ): Seq[DataPoint] = {

    val statusByTime =
      input
        .groupMap(_._1)(_._2) // Group input entries by serial time 
        .toSeq
        .sortBy(_._1)         // then sort by time to Seq[(Long,Seq[Boolean])]

    statusByTime.foldLeft(
      (
        Seq(
          DataPoint(
            0L,                     // t = 0
            1.0,                    // survival rate at t = 0 is 1.0 by definition
            false,                  // no censored entries at t = 0
            ClosedInterval(1.0,1.0) // std error vanishes at t = 0
          ),
        ),
        0.0 // accumulator for variance sum: Sum_i=1^j{di/(ni*(ni - di))}
      )
    ){
      case ((dataPoints,varAcc),(t,eventStatus)) =>

        // num of events at t
        val d = eventStatus.count(_ == true)

        // num of "patients at risk" at and after this time
        val n =
          statusByTime
            .dropWhile(_._1 < t)
            .map(_._2.size)
            .sum 

        val st = dataPoints.last.survRate * (1.0 - d.toDouble/n)

        // At the last data point, n = d if no event is censored,
        // which would lead to division by 0 in the sum entering into the variance.
        // But given that the above survival rate st becomes 0 due to d/n = 1, thus also the std error,
        // avoid NaN issues by skipping this uninformative term in the sum
        val varianceSum =
          if (d != n) varAcc + d.toDouble/(n*(n - d))
          else varAcc 

        val stdErr = st * sqrt(varianceSum)

        (
          dataPoints :+ DataPoint(
            t,
            st,
            eventStatus.forall(_ == false),
            ClosedInterval(   // Greenwood method for the confidence interval
              st - z*stdErr,
              st + z*stdErr,
            )
          ),
          varianceSum
        )

    }
    ._1

  }

}
