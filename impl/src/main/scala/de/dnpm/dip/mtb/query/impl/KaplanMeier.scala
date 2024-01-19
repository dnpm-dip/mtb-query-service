package de.dnpm.dip.mtb.query.impl


import java.time.{
  LocalDate,
  Instant,
  ZoneId
}
import java.time.temporal.ChronoUnit
import java.time.temporal.ChronoUnit.DAYS
import cats.{
  Applicative,
  Id
}
import de.dnpm.dip.model.{
  Snapshot,
  IdReference
}
import de.dnpm.dip.coding.{
  Coding,
  CodeSystemProvider
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.mtb.model.{
  MTBPatientRecord,
  MTBMedicationTherapy,
  RECIST
}
import de.dnpm.dip.mtb.query.api.KaplanMeier.{
  OS,
  PFS,
  CohortData
}



object KaplanMeierOps
{

  import scala.util.chaining._
  import ATC.extensions._


  def overallSurvival(
    snapshots: Seq[Snapshot[MTBPatientRecord]],
    timeUnit: ChronoUnit = DAYS
  ): Map[OS.Grouping,CohortData] =
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
                  .getOrElse(
                    LocalDate.ofInstant(Instant.ofEpochMilli(t),ZoneId.systemDefault)
                  ) -> false
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
    timeUnit: ChronoUnit = DAYS
  )(
    implicit atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
  ): Map[PFS.Grouping,CohortData] =
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




  object Estimator
  {

    def cohortData(
      data: Seq[(Long,Boolean)]
    ): CohortData =
      survivalRates(data)
        .pipe(
          surv => CohortData(surv,medianSurvivalTime(surv).getOrElse(0L))
        )

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
          case (survft,(t -> status)) =>

            // interval surv. rate
            val ri = {
              val i = survft.size - 1

              val di = status.count(_ == true)

              val ni = events.slice(i, events.size).map(_._2.size).sum // num of remaining data points

              (1.0 - di.toDouble/ni)
            }

            // get previous interval surv. rate
            val surv = survft.last._2

            survft :+ (t -> surv * ri)

        }

    }

    def medianSurvivalTime(
      ts: Seq[(Long,Double)]
    ): Option[Long] =
      ts.collect { case (t,sft) if sft <= 0.5 => t }
        .minOption

  }


}
