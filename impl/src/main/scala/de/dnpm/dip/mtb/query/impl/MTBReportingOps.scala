package de.dnpm.dip.mtb.query.impl


import java.time.temporal.ChronoUnit.DAYS
import scala.util.chaining._
import cats.Applicative
import de.dnpm.dip.util.DisplayLabel
import de.dnpm.dip.coding.{
  Coding,
  CodeSystemProvider
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.coding.icd.{
  ClassKinds,
  ICD,
  ICD10GM,
  ICDO3
}
import ClassKinds._
import de.dnpm.dip.model.Medications
import de.dnpm.dip.model.UnitOfTime.Weeks
import de.dnpm.dip.service.{
  Distribution,
  Entry
}
import de.dnpm.dip.service.query.ReportingOps
import de.dnpm.dip.mtb.model.{
  LevelOfEvidence,
  MTBPatientRecord,
  MTBMedicationRecommendation,
  RECIST,
}
import de.dnpm.dip.mtb.query.api.{
  GeneAlteration,
  GeneAlterations,
  MTBResultSet,
  MTBQueryCriteria,
  PFSRatio
}


trait MTBReportingOps extends ReportingOps with SurvivalOps
{

  import ICD.extensions._
  import GeneAlterationExtensions._
  import VariantExtensions._
  import de.dnpm.dip.model.Medications._  // For extensions methods on Coding[Medications]
  import Ranker.syntax._
  import Rankers._



  def therapyDistributionAndMeanDurations(
    records: Seq[MTBPatientRecord]
  )(
    implicit atc: CodeSystemProvider[ATC,cats.Id,Applicative[cats.Id]],
  ): (Distribution[Set[Coding[Medications]]],Seq[Entry[Set[Coding[Medications]],Seq[Entry[Set[Coding[Medications]],Double]]]]) = {

    val therapies =
      records
        .flatMap(_.getSystemicTherapies)
        .map(_.latest)
        .filter(_.medication.isDefined)

    val therapyDistribution =
      Distribution.byParent(
        therapies.flatMap(_.medication),
        (meds: Set[Coding[Medications]]) => meds.map(coding => coding.currentGroup.getOrElse(coding)),
      )

    val meanDurations =
      therapies
        .groupBy(_.medication.get) // first group therapies by medication (i.e. substances: ATC level 5 entries)...
        .groupBy {                 // then group again by medication group/class (ATC level 4 entries)
          case (meds,_) => meds.map(coding => coding.currentGroup.getOrElse(coding))
        }
        .map {
          case (medicationGroups,therapiesByMedication) =>
            Entry(
              medicationGroups,
              therapiesByMedication.map {
                case (meds,ths) =>
                  Entry(
                    meds,
                    ths.flatMap(_.period.flatMap(_.duration(Weeks).map(_.value)))
                      .pipe(mean(_).getOrElse(0.0))
                  )
              }
              .toSeq
            )
        }
        .toSeq

    therapyDistribution -> meanDurations

  }


  def overallDiagnosticDistributions(
    records: Seq[MTBPatientRecord]
  )(
    implicit
    icd10gm: CodeSystemProvider[ICD10GM,cats.Id,Applicative[cats.Id]],
    icdo3: CodeSystemProvider[ICDO3,cats.Id,Applicative[cats.Id]]
  ): MTBResultSet.TumorDiagnostics.Distributions = 
    MTBResultSet.TumorDiagnostics.Distributions(
      Distribution.byParent(
        records.flatMap(_.diagnoses.toList)
          .map(_.code),
        coding => coding.parentOfKind(Category).getOrElse(coding)
      ),
      Distribution.byParent(
        records.flatMap(_.getHistologyReports).map(_.results.tumorMorphology.value),
        coding => coding.parentOfKind(Block).getOrElse(coding)
      )
    )


  def recommendationDistribution(
    records: Seq[MTBPatientRecord]
  )(
    implicit
    atc: CodeSystemProvider[ATC,cats.Id,Applicative[cats.Id]]
  ): Distribution[Set[Coding[Medications]]] =
    Distribution.byParent(
      records
        .flatMap(
          _.getCarePlans
           .flatMap(_.medicationRecommendations.getOrElse(List.empty))
        )
        .map(_.medication),
      _.map(coding => coding.currentGroup.getOrElse(coding))
    )


  def recommendationsBySupportingAlteration(
    records: Seq[MTBPatientRecord],
    queriedAlterations: Option[GeneAlterations]
  )(
    implicit
    @annotation.unused atc: CodeSystemProvider[ATC,cats.Id,Applicative[cats.Id]],
  ): Seq[Entry[GeneAlteration,Distribution[Set[Coding[Medications]]]]] = {

    implicit val ranker = queriedAlterations.flatMap(GeneAlterationRanker(_))

    records.foldLeft(
      Map.empty[
        GeneAlteration,
        (
          Seq[Set[Coding[Medications]]],
          Map[GeneAlteration,Seq[Set[Coding[Medications]]]]
        )
      ]
    ){
      (acc,record) =>

        implicit val variants =
          record.getNgsReports.flatMap(_.variants)

        record
          .getCarePlans
          .flatMap(_.medicationRecommendations.getOrElse(List.empty))
          .flatMap(
            recommendation =>
              recommendation
                .medication
                .pipe { 
                  meds =>
                    recommendation
                      .supportingVariants.getOrElse(List.empty)
                      .flatMap(
                        ref => ref.variant.resolve.map(
                          variant => ref.gene match {
                            case Some(relevantGene) => variant.geneAlteration(relevantGene)
                            case None               => variant.geneAlterations
                          }
                        )
                        .getOrElse(List.empty)
                      )
                      .map(_ -> meds)
                }
          )
          .foldLeft(acc){
            case (accPr,(alteration,meds)) =>
              accPr.updatedWith(GeneAlteration(alteration.gene)){
                case Some(medSets -> children) =>
                  Some(
                    (
                      medSets :+ meds,
                      children.updatedWith(alteration){ 
                        case Some(chMedSets) => Some(chMedSets :+ meds)
                        case None            => Some(Seq(meds))
                      }
                    )
                  )

                case None => Some(Seq(meds) -> Map(alteration -> Seq(meds)))
              }
          }
    }
    .toSeq
    .optRankedBy(_._1)
    .map { 
      case (baseAlteration,(meds,children)) =>
        Entry(
          baseAlteration,
          Distribution.of(meds),
          Option.when(children.nonEmpty)(
            children
              .toSeq
              .optRankedBy(_._1)
              .map { 
                case (alteration,chMeds) => Entry(alteration,Distribution.of(chMeds))
              }
          )
        )
    }
  }


  import RECIST.{CR,PR,SD}

  // Response Rate (ratio of matched responses to total), scaled up to percentage, i.e. 0 - 100
  private def responseRate(
    matchedResponse: RECIST.Value => Boolean
  ): Seq[RECIST.Value] => Option[Int] =
    responses =>
      Option.when(responses.nonEmpty)(
        responses.count(matchedResponse).toDouble/responses.size * 100
      ) 
      .map(_.toInt)

  // Overall Response Rate: ratio of {CR,PR} to total)
  private val ORR = responseRate(Set(CR,PR)) 

  // Disease Control Rate: ratio of {CR, PR, SD} to total)
  private val DCR = responseRate(Set(CR,PR,SD))


  def therapyResponses(
    records: Seq[MTBPatientRecord],
    queryCriteria: Option[MTBQueryCriteria]
  ): Seq[MTBResultSet.TherapyResponses] = {

    implicit val ranker = queryCriteria.flatMap(TherapyResponsesRanker(_))

    records.foldLeft(
      Map.empty[
        (Coding[ICD10GM],Set[Coding[Medications]],GeneAlteration),
        (Set[Coding[LevelOfEvidence.Grading.Value]],Int,Seq[RECIST.Value],Seq[Double])
      ]
    ){ 
      (acc,record) =>

        implicit val diagnoses = record.diagnoses
        implicit lazy val recommendations = record.getCarePlans.flatMap(_.medicationRecommendations.getOrElse(List.empty))
        implicit lazy val variants = record.getNgsReports.flatMap(_.variants)
        implicit lazy val responses =
          record.getResponses
            .groupBy(_.therapy.id)
            .map {
              case (therapy,responses) => therapy -> responses.maxBy(_.effectiveDate).value.code.enumValue
            }

        val therapies =
          record.getSystemicTherapies
            .map(_.latestBy(_.recordedOn))
            .filter(_.medication.isDefined)

        therapies.foldLeft(acc){
          (acc2,therapy) =>

            val recommendationWithEntityAndGrading: Option[(MTBMedicationRecommendation,Coding[ICD10GM],Option[Coding[LevelOfEvidence.Grading.Value]])] =
              for {
                recommendation <- therapy.basedOn.flatMap(_.resolve)
                diagnosis <- recommendation.reason.flatMap(_.resolve).orElse(if (diagnoses.size == 1) Some(diagnoses.head) else None)
              } yield (
                recommendation,
                diagnosis.code,
                recommendation.levelOfEvidence.map(_.grading)
              )

            lazy val medications = therapy.medication.get
            lazy val response    = responses.get(therapy.id)
            lazy val duration    = therapy.period.flatMap(_.duration(Weeks)).map(_.value)

            recommendationWithEntityAndGrading.fold(acc2){
              case (recommendation,entity,evidenceGrading) =>

                val supportingAlterations =
                  recommendation.supportingVariants
                    .getOrElse(List.empty)
                    .flatMap(
                      ref => ref.resolveOn(variants).map(
                        variant => ref.gene match {
                          case Some(relevantGene) => variant.geneAlteration(relevantGene)
                          case None               => variant.geneAlterations
                        }
                      )
                      .getOrElse(List.empty)
                    )
                    .distinct

                supportingAlterations.foldLeft(acc2){ 
                  (acc3,alteration) =>
                    acc3.updatedWith((entity,medications,alteration))(
                      _.map {
                        case (evidenceGradings,n,recists,durations) => (
                          evidenceGradings ++ evidenceGrading,
                          n+1,
                          recists ++ response,
                          durations ++ duration
                        )
                      }
                      .orElse(Some((evidenceGrading.toSet,1, response.toSeq, duration.toSeq)))
                    )
                }    
            }
        }
    }
    .map {
      case ((entity,medications,alteration),(evidenceGradings,count,responses,durations)) =>
        MTBResultSet.TherapyResponses(
          entity,
          medications,
          alteration,
          Option(evidenceGradings).filter(_.nonEmpty),
          count,
          ORR(responses),
          DCR(responses),
          Distribution.of(responses),
          mean(durations)
        )
    }
    .toSeq
    .optRanked
  }


  def coarseTherapyResponses(
    records: Seq[MTBPatientRecord],
    queryCriteria: Option[MTBQueryCriteria]
  ): Seq[MTBResultSet.CoarseTherapyResponses] = {

    implicit val ranker = queryCriteria.flatMap(CoarseTherapyResponsesRanker(_))

    records.foldLeft(
      Map.empty[
        (Coding[ICD10GM],Set[Coding[Medications]]),
        (Set[GeneAlteration],Set[Coding[LevelOfEvidence.Grading.Value]],Int,Seq[PFSRatio.DataPoint],Seq[RECIST.Value],Seq[Double])
      ]
    ){ 
      (acc,record) =>

        implicit val diagnoses = record.diagnoses
        implicit lazy val recommendations = record.getCarePlans.flatMap(_.medicationRecommendations.getOrElse(List.empty))
        implicit lazy val variants = record.getNgsReports.flatMap(_.variants)
        implicit lazy val responses =
          record.getResponses
            .groupBy(_.therapy.id)
            .map {
              case (therapy,responses) => therapy -> responses.maxBy(_.effectiveDate).value.code.enumValue
            }

        val therapies =
          record.getSystemicTherapies
            .map(_.latestBy(_.recordedOn))
            .filter(_.medication.isDefined)

        therapies.foldLeft(acc){
          (acc2,therapy) =>

            val recommendationWithEntityAndGrading: Option[(MTBMedicationRecommendation,Coding[ICD10GM],Option[Coding[LevelOfEvidence.Grading.Value]])] =
              for {
                recommendation <- therapy.basedOn.flatMap(_.resolve)
                diagnosis <- recommendation.reason.flatMap(_.resolve).orElse(if (diagnoses.size == 1) Some(diagnoses.head) else None)
              } yield (
                recommendation,
                diagnosis.code,
                recommendation.levelOfEvidence.map(_.grading)
              )

            recommendationWithEntityAndGrading.fold(acc2){
              case (recommendation,entity,evidenceGrading) =>

                val medications = therapy.medication.get
                val response    = responses.get(therapy.id)
                val duration    = therapy.period.flatMap(_.duration(Weeks)).map(_.value)
                val supportingAlterations =
                  recommendation.supportingVariants
                    .getOrElse(List.empty)
                    .flatMap(
                      ref => ref.resolveOn(variants).map(
                        variant => ref.gene match {
                          case Some(relevantGene) => variant.geneAlteration(relevantGene)
                          case None               => variant.geneAlterations
                        }
                      )
                      .getOrElse(List.empty)
                    )

                acc2.updatedWith((entity,medications))(
                  _.map {
                    case (alterations,evidenceGradings,n,pfsRatios,recists,durations) => (
                      alterations ++ supportingAlterations,
                      evidenceGradings ++ evidenceGrading,
                      n+1,
                      pfsRatios ++ vonHoffRatio(therapy)(record,DAYS),
                      recists ++ response,
                      durations ++ duration
                    )
                  }
                  .orElse(
                    Some(
                      (
                        supportingAlterations.toSet,
                        evidenceGrading.toSet,
                        1,
                        vonHoffRatio(therapy)(record,DAYS).toList,
                        response.toSeq,
                        duration.toSeq 
                      )
                    )
                  )
                )
                
            }
        }
    }
    .map {
      case ((entity,medications),(supportingAlterations,evidenceGradings,count,pfsRatios,responses,durations)) =>
        MTBResultSet.CoarseTherapyResponses(
          entity,
          medications,
          Option(supportingAlterations).filter(_.nonEmpty),
          Option(evidenceGradings).filter(_.nonEmpty),
          count,
          pfsRatios.count(_.pfsRatio >= responderThreshold),
          ORR(responses),
          DCR(responses),
          Distribution.of(responses),
          mean(durations)
        )
    }
    .toSeq
    .optRanked
  }



  def geneAlterationInfos(
    records: Seq[MTBPatientRecord],
    queryCriteria: Option[MTBQueryCriteria]
  )(
    implicit icd10gm: CodeSystemProvider[ICD10GM,cats.Id,Applicative[cats.Id]]
  ): Seq[MTBResultSet.GeneAlterationInfo] = {

    implicit val ranker = queryCriteria.flatMap(GeneAlterationInfoRanker(_))

    records.foldLeft(
      Map.empty[(Coding[ICD10GM],GeneAlteration),(Set[Coding[ICD10GM]],Int,Int)]
    ){
      (acc,record) =>

        implicit val specimens = record.getSpecimens
        implicit val diagnoses = record.diagnoses
        implicit val recommendations = record.getCarePlans.flatMap(_.medicationRecommendations.getOrElse(List.empty))

        record.getNgsReports.foldLeft(acc){
          (acc2,report) =>

            val entity: Coding[ICD10GM] =
              report.specimen.resolve
                .flatMap(_.diagnosis.resolve)
                .get  // safe here, as referential integrity is checked upon import
                .code

            // Resolve each entity code (ICD-10) to its parent category, e.g. C71.1 -> C71      
            val entityCategory = entity.parentOfKind(Category).getOrElse(entity)

            report.variants.foldLeft(acc2){
              (acc3,variant) => variant.geneAlterations.foldLeft(acc3){

                // Accumulate by "entityCategory" as key part, but keep track of
                // the original occurring entity code in Set "entities" for use below in relevance ranking
                (acc4,alteration) =>

                  val supporting = (variant.id,alteration).isSupporting

                  acc4.updatedWith((entityCategory,alteration)){
                    case Some((entities,nTotal,nSupporting)) => Some((entities + entity, nTotal+1, if (supporting) nSupporting+1 else nSupporting))
                    case None                                => Some((Set(entity), 1, if (supporting) 1 else 0))
                  }
              }
            }
        }
    }
    .map { 
      case ((entity,alteration),(entities,nTotal,nSupporting)) => (
        MTBResultSet.GeneAlterationInfo(
          entity,
          alteration,
          nTotal,
          nSupporting,
          nSupporting > 0
        ),
        entities
      )
    }
    .toSeq
    .optRanked
    .map(_._1) // Discard the Set[Coding[ICD10GM]] of original entity codes used in ranking
  }


  def alteredGeneDistributions(
    records: Seq[MTBPatientRecord]
  ): Seq[Entry[GeneAlteration.Type.Value,Distribution[DisplayLabel[Coding[HGNC]]]]] =
    records.foldLeft(
      Map.empty[GeneAlteration.Type.Value,List[DisplayLabel[Coding[HGNC]]]]
    ){ 
      (acc,record) =>
        record.getNgsReports
          .flatMap(_.variants)
          .flatMap(_.geneAlterations)
          .collect { 
            case snv: GeneAlteration.SNV       => GeneAlteration.Type.SNV    -> DisplayLabel.of(snv.gene)
            case cnv: GeneAlteration.CNV       => GeneAlteration.Type.CNV    -> DisplayLabel.of(cnv.gene)
            case fusion: GeneAlteration.Fusion => GeneAlteration.Type.Fusion -> DisplayLabel.of(fusion.gene)
          }
          .foldLeft(acc){ 
            case (acc2,(typ,gene)) => acc2.updatedWith(typ){
              case Some(genes) => Some(gene :: genes)
              case None        => Some(List(gene))
            }         
          }
    }
    .map { case (typ,genes) => Entry(typ,Distribution.of(genes)) }
    .toSeq

}

