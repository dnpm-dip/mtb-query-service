package de.dnpm.dip.mtb.query.impl


import de.dnpm.dip.model.UnitOfTime
import scala.math.round
import cats.{
  Applicative,
  Id
}
import de.dnpm.dip.util.{
  Completer,
  DisplayLabel,
  Tree
}
import de.dnpm.dip.coding.{
  Coding,
  CodeSystem,
  CodeSystemProvider
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.UnregisteredMedication
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.coding.icd.{
  ICD10GM,
  ICDO3
}
import de.dnpm.dip.model.{
  Medications,
  Snapshot
}
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
  MTBMedicationRecommendation,
  MTBMedicationTherapy,
  RECIST,
  Variant
}
import de.dnpm.dip.mtb.query.api.{
  MTBQueryCriteria,
  MTBFilters,
  DiagnosisFilter,
  RecommendationFilter,
  TherapyFilter,
  MTBResultSet,
  KaplanMeier
}


class MTBResultSetImpl
(
  val id: Query.Id,
  val results: Seq[Query.Match[MTBPatientRecord,MTBQueryCriteria]],
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
  import MTBResultSet._
  import Completer.syntax._


  import scala.language.implicitConversions

  override implicit def toPredicate[F >: MTBFilters](f: F): MTBPatientRecord => Boolean = {

    val MTBFilters(patient,diagnoses,recommendations,therapies) =
      f.asInstanceOf[MTBFilters]


    def expandedMedicationNames(meds: Set[Set[Coding[Medications]]]): Set[Set[Tree[String]]] =
      meds.map(
        _.flatMap(
          coding => coding.system match {
            case sys if sys == Coding.System[ATC].uri =>
              coding.asInstanceOf[Coding[ATC]]
                .expand
                .map(_.map(_.display.get.toLowerCase))

            case _ => Some(Tree(coding.code.value.toLowerCase))
          }
        )
      )   

    val diagnosisFilter: MTBDiagnosis => Boolean = {

      val icd10s =
        diagnoses
          .code
          .map(
            _.flatMap(
              _.expand
            )
          )

      diag =>
        icd10s.fold(true)(_ exists (_ exists (_.code == diag.code.code)))
    }
    
    val recommendationFilter: MTBMedicationRecommendation => Boolean = { 

      val medicationNames =
        recommendations
          .medication
          .map(expandedMedicationNames)

      recommendation =>
        lazy val occurringDrugNames =
          recommendation.medication
            .flatMap(_.display)
            .map(_.toLowerCase)

        lazy val occurring: String => Boolean =
          name => occurringDrugNames exists (_ contains name)

        medicationNames.fold(true)(_ exists (_ forall (_ exists occurring)))
    }

    val therapyFilter: MTBMedicationTherapy => Boolean = { 

      val medicationNames =
        therapies
          .medication
          .map(expandedMedicationNames)

      therapy =>
        lazy val optOccurringDrugNames =
          therapy.medication
            .map(
              _.flatMap(_.display)
               .map(_.toLowerCase)
            )

        lazy val occurring: String => Boolean =
          name => optOccurringDrugNames.exists(_ exists (_ contains name))

        medicationNames.fold(true)(_ exists (_ forall (_ exists occurring)))
    }


    record => 
      patient(record.patient) &&
      record.getDiagnoses.exists(diagnosisFilter) &&
      record.getCarePlans.flatMap(_.medicationRecommendations.getOrElse(List.empty)).exists(recommendationFilter) &&
      record.getTherapies.map(_.latest).exists(therapyFilter)
  }


  override def tumorDiagnostics(
    filter: MTBFilters
  ): TumorDiagnostics = {

    val records =
      patientRecords(filter)

    TumorDiagnostics(
      overallDiagnosticDistributions(records),
      diagnosticDistributionsByVariant(records)
    )
  }

  override def medication(
    filter: MTBFilters
  ): MTBResultSet.Medication = {

    val records =
      patientRecords(filter)

    val (therapyDistribution,meanTherapyDurations) =  
      therapyDistributionAndMeanDurations(records)

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
      results.map(_.record)
    )



  import Medications._
  import RECIST._
 
  private val recistOrdering: Ordering[RECIST.Value] = {
    val ordinals =
      Map(
        CR  -> 7,
        PR  -> 6,
        MR  -> 5,
        SD  -> 4,
        PD  -> 3,
        NYA -> 2,
        NA  -> 1
      )

    new Ordering[RECIST.Value]{
      override def compare(
        r1: RECIST.Value,
        r2: RECIST.Value
      ): Int =
        ordinals(r1) compareTo ordinals(r2)
    }
  }


  val responseDistributionOrdering: Ordering[Distribution[Coding[RECIST.Value]]] =
    new Ordering[Distribution[Coding[RECIST.Value]]]{

      // Project the RECIST codes with their counts 
      // and order by decreasing order of Response "goodness",
      // i.e. from CR to PD
      private def codeCounts(
        d: Distribution[Coding[RECIST.Value]]
      ): Seq[(RECIST.Value,Int)] =
        d.elements
         .collect { 
           case Entry(RECIST(code),Count(n,_),_) => code -> n
         }
         .toSeq
         .sortBy(_._1)(recistOrdering.reverse)


      override def compare(
        d1: Distribution[Coding[RECIST.Value]],
        d2: Distribution[Coding[RECIST.Value]]
      ): Int = {
        // Zip both ordered codeCounts and
        // skip entries with equal code and count
        (codeCounts(d1) zip codeCounts(d2))
          .dropWhile {
            case ((r1,n1),(r2,n2)) => r1 == r2 && n1 == n2 
          }
          .headOption
          // Then compare by count in case of equal code, else by code 
          .map { 
            case ((r1,n1),(r2,n2)) => 
              if (r1 == r2) n1 compareTo n2
              else recistOrdering.compare(r1,r2)
          }
          // Else the zip of both distributions (determined by the shorter of both) is equal,
          // then compare their total size (more entries -> larger)
          .getOrElse(
            d1.elements.size compareTo d2.elements.size
          )
        
      }
    }
    .reverse
  

  override def therapyResponses(
    filter: MTBFilters
  ): Seq[TherapyResponseDistribution] = 
    patientRecords(filter)
      .foldLeft(
        Map.empty[
          Set[Coding[Medications]],
          (
           Set[Coding[Medications]],
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

                acc2.updatedWith(medication){ 
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
              .pipe(
                dist => dist.copy(
                  elements =
                    dist.elements.sortBy {
                      entry =>
                        val RECIST(r) = entry.key
                        r
                    }(
                      recistOrdering.reverse
                    )
                )
              )
          )
      }
      .toSeq
      .sortBy(_.responseDistribution)(responseDistributionOrdering)

}
