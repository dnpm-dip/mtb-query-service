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
    def medianSt(
      ts: Seq[DataPoint]
    ): Long =
      ts.collect {
        case DataPoint(t,surv,_,_) if surv <= 0.5 => t
      }
      .minOption
      .getOrElse(0L)


    for {
      surv <- self(input)
      mst  =  medianSt(surv)
    } yield CohortResult(
      surv,
      mst
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
    implicit
    estimator: KaplanMeierEstimator[F]
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
    projectData(
      survivalType,
      grouping,
      UnitOfTime.chronoUnit(timeUnit),
      cohort
    )
    .pipe(
      _.map {
        case (group,data) =>
          Entry(
            group,
            estimator.cohortResult(data)
          )
      }
      .toSeq
    )
    .pipe(
      SurvivalStatistics(survivalType,grouping,timeUnit,_)
    )

  }




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
            case Snapshot(record,timestamp) =>
           
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




/*
  def overallSurvival(
    snapshots: Seq[Snapshot[MTBPatientRecord]],
    timeUnit: ChronoUnit
  ): Map[OS.Grouping,CohortResult] =
    snapshots
      .flatMap { 
        case Snapshot(record,t) =>

          val (refDate,status) =
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

          record
            .diagnoses
            .toList
            .flatMap(
              diagnosis =>
                diagnosis
                  .recordedOn
                  .map(
                    diagDate =>
                      (
                        diagnosis.code,
                        timeUnit.between(diagDate,refDate),
                        status
                      )
                  )
            )
      }
      .groupMap(_._1){ case (_,t,status) => (t,status)}
      .map { 
        case (icd10,data) => icd10 -> Estimator.cohortData(data)
      }


  private val progressionRecist =
    Set(
      RECIST.PD,
      RECIST.SD
    )
    .map(Coding(_))


  def progressionFreeSurvival(
    snapshots: Seq[Snapshot[MTBPatientRecord]],
    timeUnit: ChronoUnit
  )(
    implicit atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
  ): Map[PFS.Grouping,CohortResult] =
    snapshots
      .flatMap { 
        case Snapshot(record,t) =>

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

                  medClasses  <-
                    therapy
                      .medication
                      .map(_.flatMap(_.currentGroup))
                      .map(_.flatMap(_.display))
                } yield (
                  medClasses,
                  timeUnit.between(start,refDate),
                  status
                )

            }
                  
      }
      .groupMap(_._1){ case (_,t,status) => (t,status) }
      .map { 
        case (meds,data) => meds -> Estimator.cohortData(data)
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

    val events =
     input
       .groupMap(_._1)(_._2) // Group events by serial time 
       .toSeq
       .sortBy(_._1)         // then sort by time to Seq[(Long,Seq[Boolean])]

    events
      .foldLeft(
        (
          Seq(
            DataPoint(
              0L,                     // t = 0
              1.0,                    // survival rate at t = 0 is 1.0 by definition
              false,                  // no censored events at t = 0
              ClosedInterval(1.0,1.0) // confidence interval vanishes at t = 0
            ),
          ),
          0.0  // accumulator for variance sum: Sum_i=1^j{di/(ni*(ni - di))}
        )
      ){
        case ((dataPoints,varAcc),(t,status)) =>

          val i = dataPoints.size - 1

          val di = status.count(_ == true)

          val ni = events.slice(i, events.size).map(_._2.size).sum // num of events at and after this time

          val surv = dataPoints.last.survRate * (1.0 - di.toDouble/ni)

          val varianceSum = varAcc + di.toDouble/(ni*(ni - di))

          val variance = surv*surv*varianceSum

          (
            dataPoints :+ (
              DataPoint(
                t,
                surv,
                status.forall(_ == false),
                ClosedInterval(   // Greenwood method for the confidence interval
                  surv - z*sqrt(variance),
                  surv + z*sqrt(variance),
                )
              )
            ),
            varianceSum
          )

      }
      ._1


  }


/*
    def survivalRates(
      data: Seq[(Long,Boolean)]
    ): Seq[(Long,Double)] = {

      val events =
       data.groupMap(_._1)(_._2) // Group events by serial time 
         .toSeq
         .sortBy(_._1)           // then sort by time to Seq[(T,Seq[Boolean])]

      events
        .foldLeft(
          Seq(0L -> 1.0) // survival rate at t = 0 is 1.0 by definition
        ){
          case (dataPoints,(t -> status)) =>

            // interval surv. rate
            val ri = {
              val i = dataPoints.size - 1

              val di = status.count(_ == true)

              val ni = events.slice(i, events.size).map(_._2.size).sum // num of remaining data points

              (1.0 - di.toDouble/ni)
            }

            // get previous interval surv. rate
            val surv = dataPoints.last._2

            dataPoints :+ (t -> surv * ri)

        }

    }
*/

}
