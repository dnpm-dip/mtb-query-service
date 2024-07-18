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
  Variant
}
import de.dnpm.dip.mtb.query.api.MTBResultSet.TumorDiagnostics


trait MTBReportingOps extends ReportingOps
{

  import scala.util.chaining._
  import ATC.extensions._
  import ICD.extensions._


  def therapyDistributionAndMeanDurations(
    records: Seq[MTBPatientRecord]
  )(
    implicit atc: CodeSystemProvider[ATC,Id,Applicative[Id]],
  ): (Distribution[Set[DisplayLabel[Coding[Medications]]]],Seq[Entry[Set[DisplayLabel[Coding[Medications]]],Double]]) = {

    val therapies =
      records
        .flatMap(_.getTherapies)
        .map(_.latest)
        .filter(_.medication.isDefined)

    val therapyDistribution =
      Distribution.byParentAndBy(
        therapies.flatMap(_.medication)
      )(
        _.map(coding => coding.currentGroup.getOrElse(coding)),
        _.map(DisplayLabel.of(_))
      )

    val meanDurations =
      therapies
        .groupBy(_.medication.get.map(DisplayLabel.of(_)))
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
  ): Distribution[Set[DisplayLabel[Coding[Medications]]]] =
    Distribution.byParentAndBy(
      records
        .flatMap(_.getCarePlans.flatMap(_.medicationRecommendations.getOrElse(List.empty)))
        .map(_.medication)
    )(
      _.map(coding => coding.currentGroup.getOrElse(coding)),
      _.map(DisplayLabel.of(_))
    )



  def recommendationsBySupportingVariant(
    records: Seq[MTBPatientRecord]
  )(
    implicit
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]],
  ): Seq[Entry[DisplayLabel[Variant],Distribution[Set[DisplayLabel[Coding[Medications]]]]]] =
    records.foldLeft(
      Map.empty[DisplayLabel[Variant],Seq[Set[Coding[Medications]]]]
    ){
      (acc,record) =>

        val variants =
          record
            .getNgsReports
            .flatMap(_.variants)

        record
          .getCarePlans
          .flatMap(_.medicationRecommendations.getOrElse(List.empty))
          .flatMap {
            recommendation =>
              recommendation
                .medication
                .pipe { 
                  meds =>
                    recommendation
                      .supportingVariants.getOrElse(List.empty)
                      .flatMap(_.resolveOn(variants).map(DisplayLabel.of(_)))
                      .map(_ -> meds)
                }
          }
          .foldLeft(acc){
            case (accPr,(variant,meds)) =>
              accPr.updatedWith(variant){
                _.map(_ :+ meds)
                 .orElse(Some(Seq(meds)))

              }
          }
    }
    .map { 
      case (variant,meds) =>
        Entry(
          variant,
          Distribution.byParentAndBy(
            meds
          )(
            _.map(coding => coding.currentGroup.getOrElse(coding)),
            _.map(DisplayLabel.of(_))
          )
        )
    }
    .toSeq
    .sortBy(_.key)



  def responsesByTherapy(
    records: Seq[MTBPatientRecord]
  ): Seq[Entry[Set[DisplayLabel[Coding[Medications]]],Distribution[Coding[RECIST.Value]]]] =
    records.foldLeft(
      Map.empty[Set[DisplayLabel[Coding[Medications]]],Seq[Coding[RECIST.Value]]]
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
                     _.map(DisplayLabel.of(_)) -> response.value 
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
