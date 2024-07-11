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
  Distribution,
  Entry,
Filters,
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
  Medication
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

/*
  import scala.language.implicitConversions
  override implicit def toPredicate(filter: MTBFilters): MTBPatientRecord => Boolean = {

    implicit def diagnosisFilterPredicate(f: DiagnosisFilter): MTBDiagnosis => Boolean =
      diag =>
        f.code match {
          case Some(icd10s) if icd10s.nonEmpty => icd10s.exists(_.code == diag.code.code)
          case _                               => true
        }

    record =>
      filter.patientFilter(record.patient) &&
      record.getDiagnoses.exists(filter.diagnosisFilter)
  }
*/

//  override def summary(filter: MTBFilters): Summary = {
  override def summary(
    filter: MTBPatientRecord => Boolean
  ): Summary = {

    results
      .collect { case (snp,_) if filter(snp.data) => snp }
      .pipe {
        snps =>

        val records =
          snps.map(_.data)

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

  override def medicationStats(
    filter: MTBPatientRecord => Boolean
  ): Medication =  {


    results
      .collect { 
        case (snp,_) if filter(snp.data) => snp.data 
      }
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
          Medication.TherapyResponseDistribution(
            classes,
            medications,
            supportingVariants,
            Distribution.of(responses)
          )
      }
      .toSeq
      .pipe(Medication(_))
  }      

}
