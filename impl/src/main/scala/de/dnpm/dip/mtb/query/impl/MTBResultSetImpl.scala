package de.dnpm.dip.mtb.query.impl


import java.time.temporal.ChronoUnit.DAYS
import de.dnpm.dip.model.UnitOfTime
import scala.math.round
import cats.{
  Applicative,
  Id
}
import de.dnpm.dip.util.DisplayLabel
import de.dnpm.dip.coding.{
  Coding,
  CodeSystem,
  CodeSystemProvider
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.coding.icd.{
  ICD10GM,
  ICDO3
}
import de.dnpm.dip.model.Snapshot
import de.dnpm.dip.service.query.{
  Count,
  Distribution,
  Entry,
  PatientFilter,
  PatientMatch,
  Query,
  ResultSet,
  ReportingOps
}
import de.dnpm.dip.mtb.model.{
  MTBPatientRecord,
  MTBDiagnosis,
  RECIST,
  Variant
}
import de.dnpm.dip.mtb.query.api.{
  MTBQueryCriteria,
  MTBFilters,
  DiagnosisFilter,
  MTBResultSet,
  KaplanMeier,
  TherapyResponseDistribution
}


class MTBResultSetImpl
(
  val id: Query.Id,
  val criteria: MTBQueryCriteria,
  val results: Seq[(Snapshot[MTBPatientRecord],MTBQueryCriteria)],
)(
  implicit
  atc: CodeSystemProvider[ATC,Id,Applicative[Id]],
  icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]],
  icdo3: CodeSystemProvider[ICDO3,Id,Applicative[Id]],
  kmEstimator: KaplanMeierEstimator[Id],
  kmModule: KaplanMeierModule[Id]
)  
extends MTBResultSet
with MTBReportingOps
{

  import scala.util.chaining._
  import MTBResultSet.{
    Summary,
    TumorDiagnostics,
  }

  override def summary(filter: MTBPatientRecord => Boolean): Summary = {

    val records =
      patientRecords(filter)

    val (therapyDistribution,meanTherapyDurations) =  
      therapyDistributionAndMeanDurations(records)

    Summary(
      id,
      records.size,
      ResultSet.Demographics.on(records.map(_.patient)),
      TumorDiagnostics(
        overallDiagnosticDistributions(records),
        diagnosticDistributionsByVariant(records)
      ),
      MTBResultSet.Medication(
        MTBResultSet.Medication.Recommendations(
          recommendationDistribution(records),
          recommendationsBySupportingVariant(records)
        ),
        MTBResultSet.Medication.Therapies(
          therapyDistribution,
          meanTherapyDurations,
          responsesByTherapy(records)  
        )
      )
    )

  }


  def tumorDiagnostics(
    filter: MTBPatientRecord => Boolean = _ => true
  ): MTBResultSet.TumorDiagnostics = {

    val records = patientRecords(filter)

    TumorDiagnostics(
      overallDiagnosticDistributions(records),
      diagnosticDistributionsByVariant(records)
    )
  }


  import KaplanMeier._

  override def survivalStatistics(
    survivalType: Option[SurvivalType.Value],
    grouping: Option[Grouping.Value]
  )(
    implicit env: Applicative[Id]
  ) =
    kmModule.survivalStatistics(
      survivalType,
      grouping,
      results.map(_._1)
    )



  import ATC.extensions._
  import RECIST._
 
  private val recistOrdering: Ordering[RECIST.Value] = {
    val weights =
      Map(
        CR  -> 7,
        PR  -> 6,
        MR  -> 5,
        SD  -> 4,
        PD  -> 3,
        NA  -> 2,
        NYA -> 1
      )

    new Ordering[RECIST.Value]{
      override def compare(
        r1: RECIST.Value,
        r2: RECIST.Value
      ): Int =
        weights(r1) compareTo weights(r2)
    }
  }


  val responseDistributionOrdering: Ordering[Distribution[Coding[RECIST.Value]]] =
    new Ordering[Distribution[Coding[RECIST.Value]]]{

      override def compare(
        d1: Distribution[Coding[RECIST.Value]],
        d2: Distribution[Coding[RECIST.Value]]
      ): Int = {

        val c1: Seq[(RECIST.Value,Int)] =
          d1.elements
            .collect { 
              case Entry(RECIST(code),Count(n,_),_) => code -> n
            }
            .toSeq
            .sortBy(_._1)(recistOrdering.reverse)

        val c2: Seq[(RECIST.Value,Int)] =
          d2.elements
            .collect { 
              case Entry(RECIST(code),Count(n,_),_) => code -> n
            }
            .toSeq
            .sortBy(_._1)(recistOrdering.reverse)

        c1.zip(c2)
          .dropWhile {
            case ((r1,n1),(r2,n2)) => r1 == r2 && n1 == n2 
          }
          .headOption
          .map { 
            case ((r1,n1),(r2,n2)) => 
              if (r1 == r2) n1.compareTo(n2)
              else recistOrdering.compare(r1,r2)
          }
          .getOrElse(0)
        
      }
    }
    .reverse
  

  override def therapyResponses(
    filter: MTBPatientRecord => Boolean
  ): Seq[TherapyResponseDistribution] = 
    patientRecords(filter)
      .foldLeft(
        Map.empty[
          Set[DisplayLabel[Coding[ATC]]],
          (
           Set[DisplayLabel[Coding[ATC]]],
           Set[DisplayLabel[Variant]],
           List[Coding[RECIST.Value]]
          )
        ]
      ){
        (acc,record) =>

          val recommendations =
            record
              .getCarePlans
              .flatMap(_.medicationRecommendations.getOrElse(List.empty))
              
          val variants =
            record
              .getNgsReports
              .flatMap(_.variants)

          record
            .getTherapies
            .map(_.latest)
            .view
            .filter(_.medication.isDefined)
            .foldLeft(acc){
              (acc2,therapy) =>

                val medication =
                  therapy
                    .medication.get

                val medicationClasses =
                  medication
                    .flatMap(_.currentGroup)
                    .map(DisplayLabel.of(_))

                val supportingVariants =
                  therapy
                    .basedOn
                    .flatMap(_.resolveOn(recommendations))
                    .flatMap(_.supportingVariants)
                    .getOrElse(List.empty)
                    .flatMap(_.resolveOn(variants))
                    .map(DisplayLabel.of(_))
                    .toSet
     
                val response =
                  record.getResponses
                    .filter(_.therapy.id.exists(_ == therapy.id))
                    .maxByOption(_.effectiveDate)
                    .map(_.value)

                acc.updatedWith(
                  medication.map(DisplayLabel.of(_))
                ){ 
                  case Some((classes,suppVars,responses)) => 
                    Some(
                     (
                      classes ++ medicationClasses,
                      suppVars ++ supportingVariants,
                      response.fold(responses)(_ :: responses)
                     )
                    )
                  case _ =>
                    Some(
                     (
                      medicationClasses,
                      supportingVariants,
                      response.toList
                     )
                    )  
                }    
            }

      }
      .map { 
        case (medications,(classes,supportingVariants,responses)) =>
          TherapyResponseDistribution(
            classes,
            medications,
            supportingVariants,
            Distribution.of(responses)
          )
      }
      .toSeq
      .sortBy(_.responseDistribution)(responseDistributionOrdering)

}
