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
  ConceptCount,
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


  def therapyCountsWithMeanDuration(
    records: Seq[MTBPatientRecord]
  ): (Seq[Entry[Set[String],Int]],Seq[Entry[Set[String],Double]]) = {

    records
      .flatMap(_.getMedicationTherapies)
      .flatMap(_.history.maxByOption(_.recordedOn))
      .filter(_.medication.isDefined)
      .groupBy(_.medication.get.flatMap(_.display))
      .map {
        case (meds,therapies) =>
          (
            Entry(
              meds,
              therapies.size
            ),
            Entry(
              meds,
              therapies
                .flatMap(_.period.flatMap(_.duration(Weeks)))
                .map(_.value)
                .pipe(mean(_))
            )
          )
      }
      .toSeq
      .unzip
  }


/*  
  def therapiesWithMeanDuration(
    records: Seq[MTBPatientRecord]
  ): Seq[Entry[Set[String],CountWithMeanDuration]] = {

    records
      .flatMap(_.getMedicationTherapies)
      .flatMap(_.history.maxByOption(_.recordedOn))
      .filter(_.medication.isDefined)
      .groupBy(_.medication.get.flatMap(_.display))
      .map {
        case (meds,therapies) =>
          Entry(
            meds,
            CountWithMeanDuration(
              therapies.size,
              therapies
                .flatMap(_.period.flatMap(_.duration(Weeks)))
                .map(_.value)
                .pipe(mean(_))
                .pipe(Duration(_,Weeks))
            )
          )
      }
      .toSeq
  }
*/

  def tumorEntitiesByVariant(
    records: Seq[MTBPatientRecord]
  )(
    implicit hgnc: CodeSystem[HGNC]
  ): Seq[Entry[String,Seq[ConceptCount[Coding[ICD10GM]]]]] =
    distributionsOn(
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
  ): Seq[Entry[String,Seq[ConceptCount[Set[String]]]]] =
    records.foldLeft(
      Map.empty[String,Seq[Set[String]]]
    ){
      (acc,record) =>

        implicit val variants =
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
                        _.resolve
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
          distribution(meds)
        )
    }
    .toSeq


  def responsesByTherapy(
    records: Seq[MTBPatientRecord]
  ): Seq[Entry[Set[String],Seq[ConceptCount[Coding[RECIST.Value]]]]] =
    records.foldLeft(
      Map.empty[Set[String],Seq[Coding[RECIST.Value]]]
    ){
      (acc,record) =>

        implicit val therapies =
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
                .resolve
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
          distribution(recists)
        )
    }
    .toSeq


}
