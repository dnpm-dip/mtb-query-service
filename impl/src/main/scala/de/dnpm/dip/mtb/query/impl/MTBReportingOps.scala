package de.dnpm.dip.mtb.query.impl


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
import de.dnpm.dip.coding.icd.{
  ClassKinds,
  ICD,
  ICD10GM,
  ICDO3
}
import ClassKinds._
import de.dnpm.dip.model.{
  Duration,
  Medications,
  Therapy
}
import de.dnpm.dip.model.Medications._
import de.dnpm.dip.model.UnitOfTime.Weeks
import de.dnpm.dip.service.query.{
  Count,
  ConceptCount,
  Distribution,
  Entry,
  ReportingOps
}
import de.dnpm.dip.mtb.model.{
  MTBPatientRecord,
  RECIST,
  Variant,
  SNV,
  CNV,
  Fusion,
  DNAFusion,
  RNAFusion
}
import de.dnpm.dip.mtb.query.api.MTBResultSet.TumorDiagnostics
import de.dnpm.dip.mtb.query.api.VariantCriteria


trait MTBReportingOps extends ReportingOps
{

  import scala.util.chaining._
  import ATC.extensions._
  import ICD.extensions._


  def therapyDistributionAndMeanDurations(
    records: Seq[MTBPatientRecord]
  )(
    implicit atc: CodeSystemProvider[ATC,Id,Applicative[Id]],
  ): (Distribution[Set[Coding[Medications]]],Seq[Entry[Set[Coding[Medications]],Double]]) = {

    val therapies =
      records
        .flatMap(_.getTherapies)
        .map(_.latest)
        .filter(_.medication.isDefined)

    val therapyDistribution =
      Distribution.byParent(
        therapies.flatMap(_.medication),
        (meds: Set[Coding[Medications]]) => meds.map(coding => coding.currentGroup.getOrElse(coding)),
      )

    val meanDurations =
      therapies
        .groupBy(_.medication.get)
        .map {
          case (meds,ths) =>
            Entry(
              meds,
              ths.flatMap(_.period.flatMap(_.duration(Weeks)))
                .map(_.value)
                .pipe(mean(_))
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
  ): TumorDiagnostics.Distributions = 
    TumorDiagnostics.Distributions(
      Distribution.byParent(
        records.flatMap(_.getDiagnoses)
          .map(_.code),
        coding => coding.parentOfKind(Category).getOrElse(coding)
      ),
      Distribution.byParent(
        records.flatMap(_.getHistologyReports)
          .flatMap(_.results.tumorMorphology.map(_.value)),
        coding => coding.parentOfKind(Block).getOrElse(coding)
      )
    )



  def diagnosticDistributionsByVariant(
    records: Seq[MTBPatientRecord]
  )(
    implicit
    icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]],
    icdo3: CodeSystemProvider[ICDO3,Id,Applicative[Id]]
  ): Seq[Entry[DisplayLabel[Variant],TumorDiagnostics.Distributions]] = {
    records.foldLeft(
      Map.empty[DisplayLabel[Variant],(Seq[Coding[ICD10GM]],Seq[Coding[ICDO3.M]])]
    ){
      (acc,record) =>

      val variants =
        record
          .getNgsReports
          .flatMap(_.variants)
          .map(DisplayLabel.of(_))
          .distinct   // Retain only distinct variants, to avoid duplicate counts of entities/morphologies
                      // in cases where the patient had multiple NGS reports, thus potentially redundant occurrences of most variants

      val entities =
        record.getDiagnoses
          .map(_.code)

      //TODO: Find a way to resolve morphologies in the same specimen the variant occurs in
      val morphologies =
        record.getHistologyReports
          .flatMap(_.results.tumorMorphology)
          .map(_.value)


      variants.foldLeft(acc){
        case (accPr,variant) =>
          accPr.updatedWith(variant)(
            _.map {
               case (icd10s,icdo3ms) => (entities :++ icd10s, morphologies :++ icdo3ms)
            }
            .orElse(
              Some(entities -> morphologies)
            )
          )
      }

    }
    .map {
      case (variant,(icd10s,icdo3ms)) =>
        Entry(
          variant,
          TumorDiagnostics.Distributions(
            Distribution.byParent(
              icd10s,
              coding => coding.parentOfKind(Category).getOrElse(coding)
            ),
            Distribution.byParent(
              icdo3ms,
              coding => coding.parentOfKind(Block).getOrElse(coding)
            )
          )
        )
    }
    .toSeq
    .sortBy(_.key)

  }


  def recommendationDistribution(
    records: Seq[MTBPatientRecord]
  )(
    implicit
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
  ): Distribution[Set[Coding[Medications]]] =
    Distribution.byParent(
      records
        .flatMap(_.getCarePlans.flatMap(_.medicationRecommendations.getOrElse(List.empty)))
        .map(_.medication),
      _.map(coding => coding.currentGroup.getOrElse(coding))
    )


  def recommendationsBySupportingVariant(
    records: Seq[MTBPatientRecord],
    criteria: Option[VariantCriteria]
  )(
    implicit
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]],
  ): Seq[Entry[DisplayLabel[Variant],Distribution[Set[Coding[Medications]]]]] = {

    import VariantCriteriaOps._

    def isRelevant(variant: Variant): Boolean =
      variant match {
        case snv: SNV          => criteria.flatMap(_.simpleVariants).fold(false)(_ exists(_ matches snv))
        case cnv: CNV          => criteria.flatMap(_.copyNumberVariants).fold(false)(_ exists(_ matches cnv))
        case fusion: DNAFusion => criteria.flatMap(_.dnaFusions).fold(false)(_ exists(_ matches fusion))
        case fusion: RNAFusion => criteria.flatMap(_.rnaFusions).fold(false)(_ exists(_ matches fusion))
        case rnaSeq            => false   // RNASeq currently not queryable
      }


    records.foldLeft(
      Map.empty[DisplayLabel[Variant],(Seq[Set[Coding[Medications]]],Boolean)]
    ){
      (acc,record) =>

        val variants =
          record
            .getNgsReports
            .flatMap(_.variants)

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
                      .flatMap(_ resolveOn variants)
                      .map(_ -> meds)
                }
          )
          .foldLeft(acc){
            case (accPr,(variant,meds)) =>
              accPr.updatedWith(DisplayLabel.of(variant)){
                case Some(medSets -> relevant) => Some((medSets :+ meds, relevant || isRelevant(variant)))
                case None                      => Some(Seq(meds) -> isRelevant(variant))
              }
          }
    }
    .toSeq
    .sortBy { case (_,(_,relevant)) => relevant }(Ordering[Boolean].reverse)  // reverse Ordering to have relevant entries at the beginning instead of the end
    .map { 
      case (variant,(meds,_)) =>
        Entry(
          variant,
          Distribution.of(meds)        
        )
    }
  }


  def responsesByTherapy(
    records: Seq[MTBPatientRecord]
  ): Seq[Entry[Set[Coding[Medications]],Distribution[Coding[RECIST.Value]]]] =
    records.foldLeft(
      Map.empty[Set[Coding[Medications]],Seq[Coding[RECIST.Value]]]
    ){
      (acc,record) =>

        val therapies =
          record
            .getTherapies
            .map(_.latest)

        record
          .getResponses
          .groupBy(_.therapy)
          .map {
            case (_,responses) => responses.maxBy(_.effectiveDate)
          }
          .flatMap {
            response =>
              response
                .therapy
                .resolveOn(therapies)
                .flatMap(
                  _.medication
                   .map(
                     _ -> response.value 
                   )
                )              
          }
          .foldLeft(acc){
            case (accPr,(meds,recist)) =>
              accPr.updatedWith(meds){
                _.map(_ :+ recist)
                 .orElse(Some(Seq(recist)))
              }
          }    

    }
    .map { 
      case (meds,recists) =>
        Entry(
          meds,
          Distribution.of(recists)
        )
    }
    .toSeq


}
