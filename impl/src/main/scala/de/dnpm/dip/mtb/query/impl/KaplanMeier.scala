package de.dnpm.dip.mtb.query.impl


import scala.util.chaining._
import java.time.{
  LocalDate,
  Instant,
  ZoneId
}
import java.time.temporal.ChronoUnit
import cats.{
  Applicative,
  Monad,
  Id
}
import de.dnpm.dip.model.{
  Snapshot,
  IdReference,
  ClosedInterval,
  UnitOfTime
}
import de.dnpm.dip.coding.{
  Coding,
  CodeSystemProvider
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.service.query.Entry
import de.dnpm.dip.mtb.model.{
  MTBPatientRecord,
  MTBMedicationTherapy,
  RECIST
}
import de.dnpm.dip.mtb.query.api.KaplanMeier.{
  SurvivalType,
  Grouping,
  DataPoint,
  CohortResult,
  SurvivalStatistics,
  SurvivalReport
}
import SurvivalType._
import Grouping._



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
    import cats.syntax.flatMap._

    // Compute median survival time defined as: min{ t | Surv(t) <= 0.5 }
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


trait KaplanMeierModule[F[_]]
{
  self =>

  def survivalStatistics(
    survivalType: SurvivalType.Value,
    grouping: Grouping.Value,
    timeUnit: UnitOfTime,
    cohort: Seq[Snapshot[MTBPatientRecord]]
  )(
    implicit estimator: KaplanMeierEstimator[F]
  ): F[SurvivalStatistics]



  def survivalReport(
    cohort: Seq[Snapshot[MTBPatientRecord]],
    timeUnit: UnitOfTime = UnitOfTime.Weeks,
    typesAndGroupings: Seq[(SurvivalType.Value,Grouping.Value)] =
      Seq(
        OS  -> Ungrouped,
        OS  -> ByTumorEntity,
        PFS -> ByTherapy
      )
  )(
    implicit
    estimator: KaplanMeierEstimator[F],
    F: Monad[F]
  ): F[SurvivalReport] = {

    import cats.syntax.traverse._
    import cats.syntax.functor._

    val chronoUnit =
      UnitOfTime.chronoUnit(timeUnit)

    typesAndGroupings.traverse { 
      case (survivalType,grouping) =>
        self.survivalStatistics(
          survivalType,
          grouping,
          timeUnit,
          cohort
      )
    }
    .map(SurvivalReport(_))

  }


}



object DefaultKaplanMeierModule extends KaplanMeierModule[Id]
{

  import ATC.extensions._


  override def survivalStatistics(
    survivalType: SurvivalType.Value,
    grouping: Grouping.Value,
    timeUnit: UnitOfTime,
    cohort: Seq[Snapshot[MTBPatientRecord]]
  )(
    implicit
    estimator: KaplanMeierEstimator[Id]
  ): SurvivalStatistics = {

    val chronoUnit = UnitOfTime.chronoUnit(timeUnit)

    cohort
      .flatMap(projectors(survivalType -> grouping))
      .groupMap(_._1){
        case (_,startDate,endDate,status) => chronoUnit.between(startDate,endDate) -> status
      }
      .map {
        case (group,data) =>
          Entry(
            group,
            estimator.cohortResult(data)
          )
      }
      .toSeq
      .pipe(
        SurvivalStatistics(
          Coding(survivalType),
          Coding(grouping),
          timeUnit,
          _
        )
      )

  }
/*
  override def survivalStatistics(
    survivalType: SurvivalType.Value,
    grouping: Grouping.Value,
    timeUnit: UnitOfTime,
    cohort: Seq[Snapshot[MTBPatientRecord]]
  )(
    implicit
    estimator: KaplanMeierEstimator[Id]
  ): SurvivalStatistics = {
    projectData(
      survivalType,
      grouping,
      UnitOfTime.chronoUnit(timeUnit),
      cohort
    )
    .map {
      case (group,data) =>
        Entry(
          group,
          estimator.cohortResult(data)
        )
    }
    .toSeq
    .pipe(
      SurvivalStatistics(
        Coding(survivalType),
        Coding(grouping),
        timeUnit,
        _
      )
    )

  }
*/


  implicit val atc: CodeSystemProvider[ATC,Id,Applicative[Id]] =
    ATC.Catalogs
      .getInstance[Id]
      .get


  private val progressionRecist =
    Set(
      RECIST.PD,
      RECIST.SD
    )
    .map(Coding(_))


  private val dateOfDeathOrCensoring: Snapshot[MTBPatientRecord] => (LocalDate,Boolean) = {

    case Snapshot(record,t) =>
    
      record
        .patient
        .dateOfDeath
        .map(_ -> true)
        .getOrElse(
          // 1. Censoring time strategy: fall back to date of last therapy follow-up
          record
            .getMedicationTherapies
            .flatMap(_.history.map(_.recordedOn))
            .maxOption
            // 2. Censoring time strategy: fall back to upload date
            .getOrElse(LocalDate.ofInstant(Instant.ofEpochMilli(t),ZoneId.systemDefault)) -> false
          )

  }


  private val projectors: Map[
    (SurvivalType.Value,Grouping.Value),
    Snapshot[MTBPatientRecord] => Iterable[(String,LocalDate,LocalDate,Boolean)]
  ] =
    Map(
      (OS,ByTumorEntity) -> {
        snp =>

          val (refDate,status) = dateOfDeathOrCensoring(snp)
        
          snp.data
            .diagnoses
            .toList
            .flatMap(
              diagnosis =>
                diagnosis
                  .recordedOn
                  .map(
                    diagDate =>
                      (
                        diagnosis.code.code.value,
                        diagDate,
                        refDate,
                        status
                      )
                  )
            )
      },
      (OS,Ungrouped) -> {
        snp =>
          val (refDate,status) = dateOfDeathOrCensoring(snp)

          snp.data
            .diagnoses
            .toList
            .flatMap(_.recordedOn)
            .minOption
            .map(
              date =>
                (
                  "-",
                  date,
                  refDate,
                  status
                )
            )
      },
      (PFS,ByTherapy) -> { 
        case Snapshot(record,_) =>
        
          val lastResponses =
            record
              .getResponses
              .groupBy(_.therapy)
              .collect {
                case (IdReference(therapyId,_),responses) => therapyId -> responses.maxBy(_.effectiveDate)
              }
          
          record
            .getMedicationTherapies
            .flatMap(_.history.maxByOption(_.recordedOn))
            .flatMap {
              therapy =>
          
                val (refDate,status) =
                  lastResponses
                    .get(therapy.id)
                    // 1. Look for date of latest response with recorded progression
                    .collect {
                      case response if progressionRecist contains response.value =>
                        response.effectiveDate
                    }
                    // 2. Check whether therapy was stopped due to progression and take the end or recording date
                    .orElse(
                      therapy
                        .statusReason
                        .collect { 
                          case c if c.code.value == MTBMedicationTherapy.StatusReason.Progression =>
                            therapy.period
                              .flatMap(_.endOption)
                              .getOrElse(therapy.recordedOn)
                        }
                    )
                    // 3. Use patient date of death as "progression" date
                    .orElse(record.patient.dateOfDeath)
                    .map(_ -> true)
                    // 4. Censoring: therapy recording date
                    .getOrElse(therapy.recordedOn -> false)
          
                for { 
                  start <-
                    therapy.period.map(_.start) 
          
                  medClasses <-
                    therapy
                      .medication
                      .map(_.flatMap(_.currentGroup))
                      .map(_.flatMap(_.display))
                } yield (
                  medClasses.mkString(" + "),
                  start,
                  refDate,
                  status
                )
          
            }
          }
    )

/*
  private def projectData(
    survivalType: SurvivalType.Value,
    grouping: Grouping.Value,
    timeUnit: ChronoUnit,
    snapshots: Seq[Snapshot[MTBPatientRecord]]
  ): Map[String,Seq[(Long,Boolean)]] =
    (survivalType,grouping) match {

      case (OS,ByTumorEntity) =>
        project(
          snapshots,
          timeUnit,
          {
            snp =>
            
              val (refDate,status) = dateOfDeathOrCensoring(snp)
            
              snp.data
                .diagnoses
                .toList
                .flatMap(
                  diagnosis =>
                    diagnosis
                      .recordedOn
                      .map(
                        diagDate =>
                          (
                            diagnosis.code.code.value,
                            diagDate,
                            refDate,
                            status
                          )
                      )
                )
          }
        )

      case (OS,_) => 
        project(
          snapshots,
          timeUnit,
          {
            snp =>
            
              val (refDate,status) = dateOfDeathOrCensoring(snp)

              snp.data
                .diagnoses
                .toList
                .flatMap(_.recordedOn)
                .minOption
                .map(
                  date =>
                    (
                      "-",
                      date,
                      refDate,
                      status
                    )
                )
          }
        )

      case (PFS,ByTherapy) =>
        project(
          snapshots,
          timeUnit,
          { 
            case Snapshot(record,_) =>
           
              val lastResponses =
                record
                  .getResponses
                  .groupBy(_.therapy)
                  .collect {
                    case (IdReference(therapyId,_),responses) =>
                      therapyId -> responses.maxBy(_.effectiveDate)
                  }
              
              record
                .getMedicationTherapies
                .flatMap(_.history.maxByOption(_.recordedOn))
                .flatMap {
                  therapy =>
              
                    val (refDate,status) =
                      lastResponses
                        .get(therapy.id)
                        // 1. Look for date of latest response with recorded progression
                        .collect {
                          case response if progressionRecist contains response.value =>
                            response.effectiveDate
                        }
                        // 2. Check whether therapy was stopped due to progression and take the end or recording date
                        .orElse(
                          therapy
                            .statusReason
                            .collect { 
                              case c if c.code.value == MTBMedicationTherapy.Progression =>
                                therapy.period
                                  .flatMap(_.endOption)
                                  .getOrElse(therapy.recordedOn)
                            }
                        )
                        // 3. Use patient date of death as "progression" date
                        .orElse(
                          record.patient.dateOfDeath
                        )
                        .map(_ -> true)
                        // 4. Censoring: therapy recording date
                        .getOrElse(therapy.recordedOn -> false)
              
                    for { 
                      start <-
                        therapy.period.map(_.start) 
              
                      medClasses <-
                        therapy
                          .medication
                          .map(_.flatMap(_.currentGroup))
                          .map(_.flatMap(_.display))
                    } yield (
                      medClasses.mkString(" + "),
                      start,
                      refDate,
                      status
                    )
              
                }
          }
        )

      case (PFS,_) => ???

    }


  private def project(
    snapshots: Seq[Snapshot[MTBPatientRecord]],
    timeUnit: ChronoUnit,
    projector: Snapshot[MTBPatientRecord] => Iterable[(String,LocalDate,LocalDate,Boolean)],
  ): Map[String,Seq[(Long,Boolean)]] =
    snapshots
      .flatMap(projector)
      .groupMap(_._1){
        case (_,startDate,endDate,status) => timeUnit.between(startDate,endDate) -> status
      }
*/

}



object DefaultKaplanMeierEstimator extends KaplanMeierEstimator[Id] //,Monad[Id]]
{

  import scala.math.sqrt


  private val z =
    1.96 // z-Factor for 95% confidence interval


  override def apply(
    input: Seq[(Long,Boolean)]
  )(
    implicit F: Monad[Id]
  ): Seq[DataPoint] = {

    val statusByTime =
      input
        .groupMap(_._1)(_._2) // Group input entries by serial time 
        .toSeq
        .sortBy(_._1)         // then sort by time to Seq[(Long,Seq[Boolean])]

    statusByTime
      .foldLeft(
        (
          Seq(
            DataPoint(
              0L,                     // t = 0
              1.0,                    // survival rate at t = 0 is 1.0 by definition
              false,                  // no censored entries at t = 0
              ClosedInterval(1.0,1.0) // std error vanishes at t = 0
            ),
          ),
          0.0  // accumulator for variance sum: Sum_i=1^j{di/(ni*(ni - di))}
        )
      ){
        case ((dataPoints,varAcc),(t,eventStatus)) =>

          // num of events at t
          val d =
            eventStatus.count(_ == true).toDouble

          // num of "patients at risk" at and after this time
          val n =
            statusByTime
              .dropWhile(_._1 < t)
              .map(_._2.size)
              .sum 

          val st =
            dataPoints.last.survRate * (1.0 - d/n)

          // At the last data point, n = d if no event is censored,
          // which would lead to division by 0 in the sum entering into the variance.
          // But given that the above survival rate st becomes 0 due to d/n = 1, thus also the std error,
          // avoid NaN issues by skipping this uninformative term in the sum
          val varianceSum =
            if (d != n) varAcc + d/(n*(n - d))
            else varAcc 

          val stdErr =
            st * sqrt(varianceSum)

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
