package de.dnpm.dip.mtb.query.impl


import cats.{
  Applicative,
  Id
}
import de.dnpm.dip.coding.{
  Coding,
  CodeSystem,
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
  Duration,
  Therapy
}
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
  ): (Distribution[Set[String]],Seq[Entry[Set[String],Double]]) = {

    val therapies =
      records
        .flatMap(_.getMedicationTherapies)
        .flatMap(_.history.maxByOption(_.recordedOn))
        .filter(_.medication.isDefined)

    val therapyDistribution =
      Distribution.byParentAndBy(
        therapies.flatMap(_.medication)
      )(
        _.map(coding => coding.currentGroup.getOrElse(coding)),
        _.flatMap(_.display)
      )

    val meanDurations =
      therapies
        .groupBy(_.medication.get.flatMap(_.display))
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
    hgnc: CodeSystem[HGNC],
    icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]],
    icdo3: CodeSystemProvider[ICDO3,Id,Applicative[Id]]
  ): Seq[Entry[String,TumorDiagnostics.Distributions]] = {
    records.foldLeft(
      Map.empty[String,(Seq[Coding[ICD10GM]],Seq[Coding[ICDO3.M]])]
    ){
      (acc,record) =>

      val variants =
        record
          .getNgsReports
          .flatMap(_.results.simpleVariants)
          .map(Variant.display)

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
//            Distribution.of(icd10s),
//            Distribution.of(icdo3ms)
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
  ): Distribution[Set[String]] =
    Distribution.byParentAndBy(
      records
        .flatMap(_.getCarePlans.flatMap(_.medicationRecommendations))
        .map(_.medication)
    )(
      _.map(coding => coding.currentGroup.getOrElse(coding)),
      _.flatMap(_.display)
    )
/*
    Distribution.by(
      records
        .flatMap(
          _.getCarePlans.flatMap(_.medicationRecommendations)
        )
        .map(_.medication)
    )(
      _.flatMap(_.display)
    ),
*/




  def recommendationsBySupportingVariant(
    records: Seq[MTBPatientRecord]
  )(
    implicit
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]],
    hgnc: CodeSystem[HGNC]
  ): Seq[Entry[String,Distribution[Set[String]]]] =
    records.foldLeft(
      Map.empty[String,Seq[Set[Coding[ATC]]]]
    ){
      (acc,record) =>

        val variants =
          record
            .getNgsReports
            .flatMap(_.variants)

        record
          .getCarePlans
          .flatMap(_.medicationRecommendations)
          .flatMap {
            recommendation =>
              recommendation
                .medication
                .pipe { 
                  meds =>
                    recommendation
                      .supportingEvidence
                      .flatMap(
                        _.resolveOn(variants)
                         .map(Variant.display)
                      )
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
            _.flatMap(_.display)
          )
        )
    }
    .toSeq
    .sortBy(_.key)



  def responsesByTherapy(
    records: Seq[MTBPatientRecord]
  ): Seq[Entry[Set[String],Distribution[Coding[RECIST.Value]]]] =
    records.foldLeft(
      Map.empty[Set[String],Seq[Coding[RECIST.Value]]]
    ){
      (acc,record) =>

        val therapies =
          record
            .getMedicationTherapies
            .flatMap(_.history.maxByOption(_.recordedOn))

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
                     _.flatMap(_.display) -> response.value 
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
