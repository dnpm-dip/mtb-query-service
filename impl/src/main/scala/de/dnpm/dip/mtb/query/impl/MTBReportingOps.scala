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
  Duration,
  Medications,
  Patient,
  Period,
  Snapshot,
  UnitOfTime
}
import de.dnpm.dip.model.UnitOfTime.Weeks
import de.dnpm.dip.service.query.{
  Count,
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
  DNAFusion,
  RNAFusion
}
import de.dnpm.dip.mtb.query.api.MTBResultSet
import de.dnpm.dip.mtb.query.api.VariantCriteria


trait MTBReportingOps extends ReportingOps
{

  import ICD.extensions._
  import de.dnpm.dip.model.Medications._  // For extensions methods on Coding[Medications]


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
    records: Seq[MTBPatientRecord]
  )(
    implicit
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
  ): Distribution[Set[Coding[Medications]]] = {

    Distribution.byParent(
      records
        .flatMap(
          _.getCarePlans
           .flatMap(_.medicationRecommendations.getOrElse(List.empty))
        )
        .map(_.medication),
      _.map(coding => coding.currentGroup.getOrElse(coding))
    )
  }


  def recommendationsBySupportingVariant(
    records: Seq[MTBPatientRecord],
    criteria: Option[VariantCriteria] = None
  )(
    implicit
    @annotation.unused atc: CodeSystemProvider[ATC,Id,Applicative[Id]],
  ): Seq[Entry[DisplayLabel[Variant],Distribution[Set[Coding[Medications]]]]] = {

    import VariantCriteriaOps._

    val score: Variant => Double = {
      case snv: SNV          => criteria.flatMap(_.simpleVariants).flatMap(_.map(_ score snv).maxOption).getOrElse(0.0)
      case cnv: CNV          => criteria.flatMap(_.copyNumberVariants).flatMap(_.map(_ score cnv).maxOption).getOrElse(0.0)
      case fusion: DNAFusion => criteria.flatMap(_.dnaFusions).flatMap(_.map(_ score fusion).maxOption).getOrElse(0.0)
      case fusion: RNAFusion => criteria.flatMap(_.rnaFusions).flatMap(_.map(_ score fusion).maxOption).getOrElse(0.0)
      case _                 => 0.0   // RNASeq currently not queryable
    }


    records.foldLeft(
      Map.empty[DisplayLabel[Variant],(Seq[Set[Coding[Medications]]],Double)]
    ){
      (acc,record) =>

        implicit val variants =
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
                      .flatMap(_.resolve)
                      .map(_ -> meds)
                }
          )
          .foldLeft(acc){
            case (accPr,(variant,meds)) =>
              accPr.updatedWith(DisplayLabel.of(variant)){
                case Some(medSets -> rsv) => Some((medSets :+ meds, math.max(rsv,score(variant))))
                case None                 => Some(Seq(meds) -> score(variant))
              }
          }
    }
    .toSeq
    .sortBy { case (_,(_,rsv)) => rsv }(Ordering[Double].reverse)  // reverse Ordering to sort entries by decreasing relevance score
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






import de.dnpm.dip.mtb.query.api.{
  MTBReport,
  MTBReportingCriteria
}

object ReportingExtensions
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


  implicit class PatientRecordOps(val record: MTBPatientRecord) extends AnyVal {

     import scala.language.reflectiveCalls
     
     implicit def inReportingPeriod[T <: { def issuedOn: LocalDate}](
       period: Period[LocalDate]
     ): T => Boolean =
       t => period contains t.issuedOn


    def trimmed(reportingPeriod: Period[LocalDate]): MTBPatientRecord =
      record.copy(
        ngsReports = record.ngsReports.map(_ filter reportingPeriod),
        carePlans = record.carePlans.map(_ filter reportingPeriod),
      )
  }

}


trait MTBReportCompiler extends MTBReportingOps
{

  import MTBReport._
  import ReportingExtensions._

  def apply(
    seq: Seq[Snapshot[MTBPatientRecord]],
    criteria: MTBReportingCriteria
  )(
    implicit
    atc: CodeSystemProvider[ATC,Id,Applicative[Id]],
    kaplanMeier: KaplanMeierModule[cats.Id]
  ): MTBReport = {

    val snapshots =
      seq.map(
        snp => snp.copy(
          data = snp.data.trimmed(criteria.period)
        )
      )

    val cohort =
      snapshots.map(_.data)

    MTBReport(
      criteria,
      demographics(cohort.map(_.patient)),
      Distribution.of(cohort.flatMap(_.getDiagnoses).map(_.code)),
      processStepDurations(cohort),
      recommendations(cohort),
      claimResponses(cohort),
      followUp(snapshots),
      Distribution.of(
        cohort.flatMap(_.getNgsReports)
          .map(_.sequencingType)
      )
    )
  }

  private def durations(
    seq: Seq[Long],
    timeUnit: UnitOfTime
  ): Option[Durations] =
    for {
      mean <- mean(seq)
      median <- median(seq)
    } yield Durations(
      Duration(mean,timeUnit),
      Duration(median,timeUnit),
    )


  private def demographics(patients: Seq[Patient]): Demographics = {

    val ages =
      patients.map(_.age.value)

    Demographics(
      Distribution.of(patients.map(_.gender)),
      Distribution.binned(ages,5),
      mean(ages).map(Age(_)),
      median(ages).map(Age(_))
    )
  }


  private def processStepDurations(
    cohort: Seq[MTBPatientRecord],
    timeUnit: UnitOfTime = UnitOfTime.Weeks
  ): MTBReport.ProcessStepDurations = {

    import MTBReport.ProcessStep._

    val chronoUnit =
      UnitOfTime.chronoUnit(timeUnit)


    val refToCarePlanDurations =
      cohort.flatMap {
        record =>

          val carePlans =
            record.getCarePlans
              .sortBy(_.issuedOn)

          record.episodesOfCare
            .toList
            .map(_.period.start)
            .flatMap(
              start => 
                // for lack of episode reference on CarePlan,
                // assume the first CarePlan after the episode's start date
                // to belong to this episode
                carePlans.find(_.issuedOn isAfter start)
                  .map(cp => start until (cp.issuedOn,chronoUnit))
            )
      }

    val carePlanToTherapyDurations: Seq[Long] =
      cohort.flatMap {
        record => 

          implicit val recommendations =
            record.getCarePlans
              .flatMap(_.getMedicationRecommendations)

          record.getTherapies
            // Take first therapy entry
            .map(_.history.toList.minBy(_.recordedOn)) // TODO: Consider filtering only therapies with status != NotDone
            .flatMap {
              th =>
                th.basedOn
                  .flatMap(_.resolve)
                  .map(_.issuedOn until (th.recordedOn,chronoUnit))
            }
      }
        

    Seq(
      ReferralToCarePlan -> refToCarePlanDurations,
//      ReferralToTherapy  ->
      CarePlanToTherapy  -> carePlanToTherapyDurations
    )
    .collect { 
      case (step,ts) if ts.nonEmpty =>
        Entry(
          Coding(step),
          durations(ts,timeUnit).get
        )
    }
  }


  private def recommendations(
    cohort: Seq[MTBPatientRecord],
  )(
    implicit atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
  ): Recommendations = {

    val recordsWithRecommendations =
      cohort.filter(_.getCarePlans.exists(_.hasRecommendations))

    val patientsCarePlans: Seq[(Patient,Seq[MTBCarePlan])] =
      cohort.map(
        record =>
          record.patient -> record.getCarePlans.filter(cp => cp.hasRecommendations) // && reportingPeriod.contains(cp.issuedOn))
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
      ),
      recommendationsBySupportingVariant(
        recordsWithRecommendations,
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


  private def claimResponses(
    cohort: Seq[MTBPatientRecord],
  ): Seq[Entry[Coding[ClaimResponse.Status.Value],ClaimResponses]] = {


    implicit val claims =
      cohort.flatMap(_.getClaims) 

    implicit val recommendations =
      cohort
        .flatMap(_.getCarePlans)
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


  private def followUp(
    snapshots: Seq[Snapshot[MTBPatientRecord]],
  )(
    implicit kaplanMeier: KaplanMeierModule[cats.Id]
  ): FollowUp = {

    val cohort =
      snapshots.map(_.data)

    implicit val recommendationsInPeriod =
      cohort
        .flatMap(_.getCarePlans)
        .flatMap(_.getMedicationRecommendations)

    val therapies =
      cohort.flatMap(_.getTherapies.map(_.latest))

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
                  .flatMap(_.statusReason)
              )
            )
        }
        .toSeq,
      kaplanMeier.pfsRatioReport(snapshots)  
    )  

  }

}
