package de.dnpm.dip.mtb.query.impl


import java.time.LocalDate
import scala.util.chaining._
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
  Age,
  Gender,
  Duration,
  Medications,
  Patient,
  Period,
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
  ClaimResponse,
  MTBPatientRecord,
  MTBCarePlan,
  RECIST,
  Variant,
  SNV,
  CNV,
  Fusion,
  DNAFusion,
  RNAFusion
}
import de.dnpm.dip.mtb.query.api.MTBResultSet
import de.dnpm.dip.mtb.query.api.VariantCriteria


trait MTBReportingOps extends ReportingOps
{

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
              ths.flatMap(_.period.flatMap(_.duration(Weeks).map(_.value)))
                .pipe(mean(_).getOrElse(0.0))
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
  ): Seq[Entry[DisplayLabel[Variant],MTBResultSet.TumorDiagnostics.Distributions]] = {
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
          MTBResultSet.TumorDiagnostics.Distributions(
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
    records: Seq[MTBPatientRecord],
    period: Option[Period[LocalDate]] = None
  )(
    implicit
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
  ): Distribution[Set[Coding[Medications]]] = {

    val inRelevantPeriod: MTBCarePlan => Boolean =
      cp => period.fold(true)(_ contains cp.issuedOn)

    Distribution.byParent(
      records
        .flatMap(
          _.getCarePlans
           .filter(inRelevantPeriod)
           .flatMap(_.medicationRecommendations.getOrElse(List.empty))
        )
        .map(_.medication),
      _.map(coding => coding.currentGroup.getOrElse(coding))
    )
  }


  def recommendationsBySupportingVariant(
    records: Seq[MTBPatientRecord],
    criteria: Option[VariantCriteria] = None,
    period: Option[Period[LocalDate]] = None
  )(
    implicit
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]],
  ): Seq[Entry[DisplayLabel[Variant],Distribution[Set[Coding[Medications]]]]] = {

    import VariantCriteriaOps._

    val inRelevantPeriod: MTBCarePlan => Boolean =
      cp => period.fold(true)(_ contains cp.issuedOn)

    val isRelevant: Variant => Boolean = {
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

        implicit val variants =
          record
            .getNgsReports
            .flatMap(_.variants)

        record
          .getCarePlans
          .filter(inRelevantPeriod)
          .flatMap(_.medicationRecommendations.getOrElse(List.empty))
          .flatMap(
            recommendation =>
              recommendation
                .medication
                .pipe { 
                  meds =>
                    recommendation
                      .supportingVariants.getOrElse(List.empty)
                      .flatMap(_.resolve)
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






import de.dnpm.dip.mtb.query.api.MTBReport

object ModelExtensions
{
  implicit class CarePlanOps(val cp: MTBCarePlan) extends AnyVal
  {
    def hasMedicationRecommendations =
      cp.medicationRecommendations.exists(_.nonEmpty)

    def hasStudyEnrollmentRecommendations =
      cp.studyEnrollmentRecommendations.exists(_.nonEmpty)

    def hasRecommendations =
      hasMedicationRecommendations || hasStudyEnrollmentRecommendations

    def getMedicationRecommendations =
      cp.medicationRecommendations.getOrElse(List.empty)

    def getStudyEnrollmentRecommendations =
      cp.studyEnrollmentRecommendations.getOrElse(List.empty)
  }
}


trait MTBReportCompiler extends MTBReportingOps
{

  import MTBReport._
  import ModelExtensions._


  def demographics(patients: Seq[Patient]): Demographics = {

    val ages =
      patients.map(_.age.value)

    Demographics(
      Distribution.of(patients.map(_.gender)),
      Distribution.binned(ages,5),
      Age(mean(ages).getOrElse(0.0)),
//      Age(median(ages))
    )
  }


  def recommendations(
    cohort: Seq[MTBPatientRecord],
    period: Period[LocalDate]
  )(
    implicit
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
  ): Recommendations = {


    val recordsWithRecommendations =
      cohort.filter(_.getCarePlans.exists(_.hasRecommendations))

    val patientsCarePlans: Seq[(Patient,Seq[MTBCarePlan])] =
      cohort.map(
        record =>
          record.patient -> record.getCarePlans.filter(cp => cp.hasRecommendations && period.contains(cp.issuedOn))
        )
        .filter(_._2.nonEmpty)

    val (patientsInStudies,studyEnrollmentRecommendations) =
      patientsCarePlans.collect { 
        case (patient,carePlans) if carePlans.exists(_.hasStudyEnrollmentRecommendations) =>
          patient -> carePlans.flatMap(_.getStudyEnrollmentRecommendations)
      }
      .unzip


    implicit val diagnoses =
      cohort.flatMap(_.getDiagnoses)


    Recommendations(
      Count.of(
        n     = recordsWithRecommendations.size,
        total = cohort.size
      ),
      demographics(recordsWithRecommendations.map(_.patient)),
      Distribution.of(
        patientsCarePlans
          .flatMap(_._2)
          .flatMap(_.getMedicationRecommendations)
          .flatMap(_.indication.flatMap(_.resolve))
          .map(_.code)
      ),
      recommendationDistribution(
        recordsWithRecommendations,
        Some(period)
      ),
      recommendationsBySupportingVariant(
        records = recordsWithRecommendations,
        period = Some(period)
      ),
      Distribution.of(
        patientsCarePlans.flatMap(_._2)
          .flatMap(_.getMedicationRecommendations)
          .flatMap(_.levelOfEvidence.map(_.grading))
      ),
      Recommendations.StudyRecommendations(
        demographics(patientsInStudies),
        Distribution.of(
          studyEnrollmentRecommendations
            .flatten
            .flatMap(_.studies.getOrElse(List.empty))
        )
      )
    )
  }


  def claimResponses(
    cohort: Seq[MTBPatientRecord],
    period: Period[LocalDate]
  ): Seq[Entry[Coding[ClaimResponse.Status.Value],ClaimResponses]] = {


    implicit val claims =
      cohort.flatMap(_.getClaims) 

    implicit val recommendations =
      cohort
        .flatMap(_.getCarePlans.filter(cp => period.contains(cp.issuedOn)))
        .flatMap(_.getMedicationRecommendations)

    val allClaimResponses =
      cohort.flatMap(_.getClaimResponses)

    val counter =
      Count.total(allClaimResponses.size)

    allClaimResponses
      .groupBy(_.status)
      .map { 
        case (status,crs) =>
          Entry(
            status,
            ClaimResponses(
              counter(crs.size),
              Distribution.of(
                crs.flatMap(_.claim.resolve)
                  .flatMap(_.recommendation.resolve)
                  .flatMap(_.levelOfEvidence.map(_.grading))
              )
            )
          )
      }
      .toSeq

  }


  def followUp(
    cohort: Seq[MTBPatientRecord],
    period: Period[LocalDate]
  ): FollowUp = {

    implicit val recommendationsInPeriod =
      cohort
        .flatMap(_.getCarePlans.filter(rec => period contains rec.issuedOn))
        .flatMap(_.getMedicationRecommendations)

    val therapies =
      cohort.flatMap(
        _.getTherapies.map(_.latest)
      )

    FollowUp(
      Count.of(
        n     = cohort.count(_.patient.dateOfDeath.isDefined),
        total = cohort.size
      ),
      therapies
        .groupBy(_.status)
        .map { 
          case (status,ths) =>
            Entry(
              status,
              Distribution.of(
                ths.filter(_.basedOn.flatMap(_.resolve).isDefined)
//                ths.filter(_.basedOn.flatMap(_.resolve).exists(rec => period contains rec.issuedOn))
                  .flatMap(_.statusReason)
              )
            )
        }
        .toSeq

    )  

  }

}
