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
import de.dnpm.dip.coding.icd.ClassKinds.Category
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
  Variant,
  SNV,
  CNV,
  DNAFusion,
  RNAFusion
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
  val queryCriteria: Option[MTBQueryCriteria],
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
  import scala.language.implicitConversions

  import MTBResultSet._
  import Completer.syntax._
  import Medications._
  import de.dnpm.dip.coding.icd.ICD.extensions._



  override implicit def toPredicate[F >: MTBFilters](
    mtbFilter: F
  ): MTBPatientRecord => Boolean = {

    import PatientFilter.Extensions._
    import MTBFilterExtensions._

    val MTBFilters(patientFilter,diagnosisFilter,recommendationFilter,therapyFilter) =
      mtbFilter.asInstanceOf[MTBFilters]

    record =>
      patientFilter(record.patient) &&
      diagnosisFilter(record.getDiagnoses) &&
      recommendationFilter(record.getCarePlans.flatMap(_.medicationRecommendations.getOrElse(List.empty))) &&
      therapyFilter(record.getTherapies.map(_.latest))

  }


  override lazy val defaultFilter: MTBFilters = {

    val records =
      patientRecords(_ => true)

    MTBFilters(
      PatientFilter.on(records),
      DiagnosisFilter(
        Some(
          records
            .flatMap(_.getDiagnoses.map(_.code))
            .toSet
            .pipe(
              icd10s => icd10s ++ icd10s.flatMap(_.parentOfKind(Category))
            )
        )
      ),
      RecommendationFilter(
        Some(
          records.flatMap(_.getCarePlans)
            .flatMap(_.medicationRecommendations.getOrElse(List.empty))
            .map(_.medication)
            .toSet
            .pipe(
              meds => meds ++ meds.map(_.flatMap(_.currentGroup))
            )
        )
      ),
      TherapyFilter(
        Some(
          records.flatMap(_.getTherapies)
            .map(_.latest)
            .flatMap(_.medication)
            .toSet
            .pipe(
              meds => meds ++ meds.map(_.flatMap(_.currentGroup))
            )
        )
      )
    )
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
        recommendationsBySupportingVariant(records,queryCriteria.flatMap(_.variants))
      ),
      MTBResultSet.Medication.Therapies(
        therapyDistribution,
        meanTherapyDurations
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

          implicit val recommendations =
            record
              .getCarePlans
              .flatMap(_.medicationRecommendations.getOrElse(List.empty))

          implicit val variants =
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
                    .flatMap(_.resolve)
                    .flatMap(_.supportingVariants)
                    .getOrElse(List.empty)
                    .flatMap(_.resolve)
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



  override def therapyResponsesBySupportingVariant(
    filter: MTBFilters
  ): Seq[MTBResultSet.TherapyResponses] = {

    import VariantCriteriaOps._

    val variantCriteria =
      queryCriteria.flatMap(_.variants)

    val score: Variant => Double = {
      case snv: SNV          => variantCriteria.flatMap(_.simpleVariants).flatMap(_.map(_ score snv).maxOption).getOrElse(0.0)
      case cnv: CNV          => variantCriteria.flatMap(_.copyNumberVariants).flatMap(_.map(_ score cnv).maxOption).getOrElse(0.0)
      case fusion: DNAFusion => variantCriteria.flatMap(_.dnaFusions).flatMap(_.map(_ score fusion).maxOption).getOrElse(0.0)
      case fusion: RNAFusion => variantCriteria.flatMap(_.rnaFusions).flatMap(_.map(_ score fusion).maxOption).getOrElse(0.0)
      case rnaSeq            => 0.0   // RNASeq currently not queryable
    }


    patientRecords(filter)
      .foldLeft(
        Map.empty[
          DisplayLabel[Variant],
          (Double, Map[Set[Coding[Medications]],(Set[Coding[Medications]],List[Coding[RECIST.Value]])])
        ]
      ){
        (acc,record) =>

          implicit val recommendations =
            record
              .getCarePlans
              .flatMap(_.medicationRecommendations.getOrElse(List.empty))
              
          implicit val variants =
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
                    .flatMap(_.resolve)
                    .flatMap(_.supportingVariants)
                    .getOrElse(List.empty)
                    .flatMap(_.resolve)
     
                val response =
                  record.getResponses
                    .filter(_.therapy.id.exists(_ == therapy.id))
                    .maxByOption(_.effectiveDate)
                    .map(_.value)

                supportingVariants.foldLeft(acc2){
                  (acc3,variant) =>
                    acc3.updatedWith(DisplayLabel.of(variant)){
                      _.map { 
                        case (rsv,acc4) =>
                          (
                            math.max(rsv,score(variant)),
                            acc4.updatedWith(medication){
                              _.map { 
                                case (medClasses, responses) => (medClasses, responses ++ response)
                              }
                              .orElse(Some(medicationClasses -> response.toList))
                            }
                          )
                      }
                      .orElse(
                        Some(score(variant) -> Map(medication -> (medicationClasses,response.toList)))
                      )
                    }

                }   
              }   
      }
      .toSeq
      .sortBy {
        case (_,(rsv,_)) => rsv 
      }(
        Ordering[Double].reverse  // reverse Ordering to sort entries by decreasing relevance score
      )
      .flatMap { 
        case (supportingVariant,(_,acc)) =>
          acc
            .toSeq
            .map {
              case (medications,(medClasses,responses)) =>
                TherapyResponses(
                  supportingVariant,
                  medClasses,
                  medications,
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
            .sortBy(_.responseDistribution)(responseDistributionOrdering)
      }

  }

}
