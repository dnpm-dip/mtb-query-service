package de.dnpm.dip.mtb.query.impl


import scala.util.chaining._
import cats.{
  Applicative,
  Id
}
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
import de.dnpm.dip.model.{
  Medications,
}
import de.dnpm.dip.model.UnitOfTime.Weeks
import de.dnpm.dip.service.{
  Distribution,
  Entry
}
import de.dnpm.dip.service.query.ReportingOps
import de.dnpm.dip.mtb.model.{
  MTBPatientRecord,
  MTBMedicationRecommendation,
  RECIST,
}
import de.dnpm.dip.mtb.query.api.{
  GeneAlteration,
  GeneAlterations,
  MTBResultSet,
  MTBQueryCriteria
}


trait MTBReportingOps extends ReportingOps
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
    implicit atc: CodeSystemProvider[ATC,Id,Applicative[Id]],
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
    icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]],
    icdo3: CodeSystemProvider[ICDO3,Id,Applicative[Id]]
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


  def diagnosticDistributionsByAlteration(
    records: Seq[MTBPatientRecord],
    queriedAlterations: Option[GeneAlterations] 
  )(
    implicit
    icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]],
    icdo3: CodeSystemProvider[ICDO3,Id,Applicative[Id]]
  ): Seq[Entry[GeneAlteration,MTBResultSet.TumorDiagnostics.Distributions]] = {
   
    implicit val ranker = queriedAlterations.flatMap(GeneAlterationRanker(_))

    records.foldLeft(
      Map.empty[
        GeneAlteration,
        (
          Seq[Coding[ICD10GM]],
          Seq[Coding[ICDO3.M]],
          Map[GeneAlteration,(Seq[Coding[ICD10GM]],Seq[Coding[ICDO3.M]])]
        )
      ]
    ){
      (acc,record) =>

      val alterations =
        record
          .getNgsReports
          .flatMap(_.variants.flatMap(_.geneAlterations))
          .distinct   // Retain only distinct alterations, to avoid duplicate counts of entities/morphologies
                      // in cases where the patient had multiple NGS reports, thus potentially redundant occurrences of most alterations

      val entities =
        record.diagnoses.map(_.code).toList

      //TODO: Find a way to resolve morphologies in the same specimen the alteration occurs in
      val morphologies =
        record.getHistologyReports
          .map(_.results.tumorMorphology.value)

      alterations.foldLeft(acc){
        case (accPr,alteration) =>
          accPr.updatedWith(GeneAlteration(alteration.gene)){
            case Some((icd10s,icdo3ms,children)) =>
              Some(
                (
                  entities :++ icd10s,
                  morphologies :++ icdo3ms,
                  children.updatedWith(alteration){ 
                    case Some(chIcd10s -> chIcdo3ms) => Some((entities :++ chIcd10s, morphologies :++ chIcdo3ms))
                    case None => Some((entities,morphologies))
                  }
                )
              )
              
            case None =>
              Some(
                (
                  entities,
                  morphologies,
                  Map(alteration -> (entities,morphologies))
                )
              )
          }
      }
    }
    .toSeq
    .optRankedBy(_._1)
    .map {
      case (alteration,(icd10s,icdo3ms,children)) =>
        Entry(
          alteration,
          MTBResultSet.TumorDiagnostics.Distributions(
            Distribution.byParent(icd10s,  coding => coding.parentOfKind(Category).getOrElse(coding)),
            Distribution.byParent(icdo3ms, coding => coding.parentOfKind(Block).getOrElse(coding))
          ),
          Option.when(children.nonEmpty)(
            children
              .toSeq
              .optRankedBy(_._1)
              .map {
                case (childAlteration,(chIcd10s,chIcdo3ms)) =>
                  Entry(
                    childAlteration,
                    MTBResultSet.TumorDiagnostics.Distributions(
                      Distribution.byParent(chIcd10s,  coding => coding.parentOfKind(Category).getOrElse(coding)),
                      Distribution.byParent(chIcdo3ms, coding => coding.parentOfKind(Block).getOrElse(coding))
                    )
                  )
              }
          )
        )
    }
  }


  def recommendationDistribution(
    records: Seq[MTBPatientRecord]
  )(
    implicit
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
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
    @annotation.unused atc: CodeSystemProvider[ATC,Id,Applicative[Id]],
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


  // Overall Response Rate (ratio of CR or PR responses to total),
  // scaled up to percentage, i.e. 0 - 100
  private val PositiveResponse = Set(RECIST.CR,RECIST.PR) 

  private val ORR: Seq[RECIST.Value] => Int =
    responses =>
      if (responses.nonEmpty) ((responses count PositiveResponse).toDouble/responses.size * 100).toInt
      else 0

  def therapyResponses(
    records: Seq[MTBPatientRecord],
    queryCriteria: Option[MTBQueryCriteria]
  ): Seq[MTBResultSet.TherapyResponses] = {

    implicit val ranker = queryCriteria.flatMap(TherapyResponsesRanker(_))

    records.foldLeft(
      Map.empty[
        (Coding[ICD10GM],Set[Coding[Medications]],GeneAlteration),
        (Int,Seq[RECIST.Value],Seq[Double])
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

            val recommendationEntity: Option[(MTBMedicationRecommendation,Coding[ICD10GM])] =
              for {
                recommendation <- therapy.basedOn.flatMap(_.resolve)
                diagnosis <- recommendation.reason.flatMap(_.resolve)
              } yield (recommendation, diagnosis.code)

            lazy val medications = therapy.medication.get
            lazy val response    = responses.get(therapy.id)
            lazy val duration    = therapy.period.flatMap(_.duration(Weeks)).map(_.value)

            recommendationEntity.fold(acc2){
              case (recommendation,entity) =>

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
                        case (n,recists,durations) => (n+1, recists ++ response, durations ++ duration)
                      }
                      .orElse(Some((1, response.toSeq, duration.toSeq)))
                    )
                }    
            }
        }
    }
    .map {
      case ((entity,medications,alteration),(n,responses,durations)) =>
        MTBResultSet.TherapyResponses(
          entity,
          medications,
          alteration,
          n,
          ORR(responses),
          Distribution.of(responses),
          mean(durations).getOrElse(0.0)
        )
    }
    .toSeq
    .optRanked
  }


  def geneAlterationInfos(
    records: Seq[MTBPatientRecord],
    queryCriteria: Option[MTBQueryCriteria]
  )(
    implicit icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]]
  ): Seq[MTBResultSet.GeneAlterationInfo] = {

    implicit val ranker = queryCriteria.flatMap(GeneAlterationInfoRanker(_))

    records.foldLeft(
      Map.empty[(Coding[ICD10GM],GeneAlteration),(Set[Coding[ICD10GM]],Int,Boolean)]
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
              (acc3,variant) =>
                variant.geneAlterations.foldLeft(acc3){

                  // Accumulate by "entityCategory" as key part, but keep track of
                  // the original occurring entity code in Set "entities" for use below in relevance ranking
                  (acc4,alteration) => acc4.updatedWith(
                    (entityCategory,alteration)
                  ){
                    case Some((entities,n,supporting)) => Some((entities + entity, n+1, supporting || (variant.id,alteration).isSupporting))
                    case None                          => Some((Set(entity), 1, (variant.id,alteration).isSupporting))
                  }
                }
            }
        }
    }
    .map { 
      case ((entity,alteration),(entities,n,supporting)) =>
        MTBResultSet.GeneAlterationInfo(entity,alteration,n,supporting) -> entities
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

