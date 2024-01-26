package de.dnpm.dip.mtb.query.impl



import de.dnpm.dip.coding.{
  Coding,
  CodeSystem
}
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.coding.icd.ICD10GM
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


trait MTBReportingOps extends ReportingOps
{

  import scala.util.chaining._


  def therapyDistributionAndMeanDurations(
    records: Seq[MTBPatientRecord]
  ): (Distribution[Set[String]],Seq[Entry[Set[String],Double]]) = {

    val therapies =
      records
        .flatMap(_.getMedicationTherapies)
        .flatMap(_.history.maxByOption(_.recordedOn))
        .filter(_.medication.isDefined)

    val counter =
      Count.total(therapies.size)

    
    therapies
      .groupBy(_.medication.get.flatMap(_.display))
      .map {
        case (meds,ths) =>
          (
            ConceptCount(
              meds,
              counter(ths.size)
            ),
            Entry(
              meds,
              ths
                .flatMap(_.period.flatMap(_.duration(Weeks)))
                .map(_.value)
                .pipe(mean(_))
            )
          )
      }
      .toSeq
      .unzip
      .pipe {
        case (counts,meanDurations) =>
          Distribution(therapies.size,counts) -> meanDurations
      }
    
  }


  def tumorEntitiesByVariant(
    records: Seq[MTBPatientRecord]
  )(
    implicit hgnc: CodeSystem[HGNC]
  ): Seq[Entry[String,Distribution[Coding[ICD10GM]]]] =
    Distribution.associatedOn(
      records
    )(
      _.getNgsReports
       .flatMap(_.variants)
       .map(Variant.display),
      _.diagnoses
       .toList
       .map(_.code) 
    )


  def recommendationsBySupportingVariant(
    records: Seq[MTBPatientRecord]
  )(
    implicit hgnc: CodeSystem[HGNC]
  ): Seq[Entry[String,Distribution[Set[String]]]] =
    records.foldLeft(
      Map.empty[String,Seq[Set[String]]]
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
                .flatMap(_.display)
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
          Distribution.of(meds)
        )
    }
    .toSeq


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
