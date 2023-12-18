package de.dnpm.dip.mtb.query.impl


import cats.{
  Applicative,
  Id
}
import de.dnpm.dip.util.Completer
import de.dnpm.dip.coding.{
  Coding,
  CodeSystem,
  CodeSystemProvider
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.{
  ICD10GM,
  ICDO3
}
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.model.{
  Patient,
  Site,
  Reference,
  Resolver,
//  IdReference,
}
import de.dnpm.dip.mtb.model._
import de.dnpm.dip.mtb.query.api.MTBQueryCriteria



trait Completers
{

  import Completer.syntax._
  import MTBMedicationTherapy.statusReasonCodeSystem


  val localSite: Coding[Site]

  implicit val hgnc: CodeSystem[HGNC]

  implicit val atc: CodeSystemProvider[ATC,Id,Applicative[Id]]

  implicit val icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]]

  val icdo3: ICDO3.Catalogs[Id,Applicative[Id]]

  implicit val whoGrading: CodeSystemProvider[WHOGrading,Id,Applicative[Id]] = 
    new WHOGrading.Provider.Facade[Id]




  private implicit val patientCompleter: Completer[Patient] =
    Completer.of(
      pat =>
        pat.copy(
          gender       = pat.gender.complete,
          managingSite = Some(localSite)
        )
    )

  implicit val mtbPatientRecordCompleter: Completer[MTBPatientRecord] = {

    implicit val episodeCompleter: Completer[MTBEpisode] =
      Completer.of(
        episode => episode.copy(
         status = episode.status.complete
        )
      )

    implicit val diagnosisCompleter: Completer[MTBDiagnosis] =
      Completer.of(
        diagnosis => diagnosis.copy(
          code = diagnosis.code.complete, 
          whoGrading               = diagnosis.whoGrading.complete, 
          stageHistory             = diagnosis.stageHistory.map(st => st.copy(stage = st.stage.complete)), 
          guidelineTreatmentStatus = diagnosis.guidelineTreatmentStatus.complete,
          topography =
            diagnosis.topography.flatMap(
              coding =>
                icdo3.topography(coding.version.getOrElse(icdo3.latestVersion))
                  .map(Coding.completeByCodeSystem(_))
                  .map(coding.complete(_))
                
            )
        )
      )

    implicit val therapyCompleter: Completer[MTBMedicationTherapy] =
      Completer.of(
        therapy => therapy.copy(
          status       = therapy.status.complete,
          statusReason = therapy.statusReason.complete,
          medication   = therapy.medication.complete
        )
      )

    implicit val procedureCompleter: Completer[OncoProcedure] =
      Completer.of(
        procedure => procedure.copy(
          code         = procedure.code.complete,
          status       = procedure.status.complete,
          statusReason = procedure.statusReason.complete
        )
      )

    implicit val ecogCompleter: Completer[PerformanceStatus] =
      Completer.of(
        ecog => ecog.copy(value = ecog.value.complete)
      )

    implicit val specimenCompleter: Completer[TumorSpecimen] =
      Completer.of(
        specimen => specimen.copy(
          `type` = specimen.`type`.complete,
          collection = specimen.collection.map(
            coll =>
              coll.copy(
                method       = coll.method.complete,
                localization = coll.localization.complete
              )
          )
        )
      )

    implicit val tumorCellContentompleter: Completer[TumorCellContent] =
      Completer.of(
        tcc => tcc.copy(
          method = tcc.method.complete
        )
      )
    implicit val histologyReportCompleter: Completer[HistologyReport] =
      Completer.of(
        report => report.copy(
          results = report.results.copy(
            tumorCellContent = report.results.tumorCellContent.complete,
            tumorMorphology =
              report.results.tumorMorphology.flatMap(
                obs =>
                  icdo3.morphology(obs.value.version.getOrElse(icdo3.latestVersion))
                    .map(Coding.completeByCodeSystem(_))
                    .map(cmpl =>
                      obs.copy(value = obs.value.complete(cmpl))
                    )
              )
          )
        )
      )


    implicit val ihcReportCompleter: Completer[IHCReport] = {

      implicit val proteinExpressionCompleter: Completer[ProteinExpression] =
        Completer.of(
          obs => obs.copy(
            protein = obs.protein.complete,
            value   = obs.value.complete,  
            icScore = obs.icScore.complete,
            tcScore = obs.tcScore.complete
          )
        )

      Completer.of(
        report => report.copy(
          proteinExpressionResults = report.proteinExpressionResults.complete,
          msiMmrResults            = report.msiMmrResults.complete
        )
      )

    }



    implicit val snvCompleter: Completer[SNV] =
      Completer.of(
        snv => snv.copy(
          chromosome = snv.chromosome.complete,
          gene       = snv.gene.complete,
          interpretation = snv.interpretation.complete
        )
      )

    implicit val cnvCompleter: Completer[CNV] =
      Completer.of(
        cnv => cnv.copy(
          chromosome            = cnv.chromosome.complete,
          `type`                = cnv.`type`.complete,
          reportedAffectedGenes = cnv.reportedAffectedGenes.complete,
          copyNumberNeutralLoH  = cnv.copyNumberNeutralLoH.complete,
        )
      )


    implicit val ngsReportCompleter: Completer[NGSReport] =
      Completer.of(
        report => report.copy(
          results = report.results.copy(
            tumorCellContent   = report.results.tumorCellContent.complete,
            tmb                = report.results.tmb.map(obs => obs.copy(interpretation = obs.interpretation.complete)),
            simpleVariants     = report.results.simpleVariants.complete,  
            copyNumberVariants = report.results.copyNumberVariants.complete,
            //TODO: Fusions, RNASeq
          )
        )
      )

    implicit def medicationRecommendationCompleter(
      implicit variants: List[Variant]
    ): Completer[MTBMedicationRecommendation] = {

      implicit val resolver =
        Resolver.on(variants)

      Completer.of(
        recommendation => recommendation.copy(
          levelOfEvidence = recommendation.levelOfEvidence.map(
            loe => loe.copy(
              grading   = loe.grading.complete,
              addendums = loe.addendums.complete
            )
          ),
          priority        = recommendation.priority.complete,
          medication      = recommendation.medication.complete,
          supportingEvidence =
            recommendation.supportingEvidence.flatMap {
              _.resolve
               .map(
                 variant => Reference(variant.id,Some(Variant.display(variant)))
               )
            }
        )
      )
    }

    implicit def carePlanCompleter(
      implicit variants: List[Variant]
    ): Completer[MTBCarePlan] =
      Completer.of(
        carePlan => carePlan.copy(
          statusReason                     = carePlan.statusReason.complete,
          medicationRecommendations        = carePlan.medicationRecommendations.complete,
          geneticCounselingRecommendations = carePlan.geneticCounselingRecommendations.map(
            rec => rec.copy(reason = rec.reason.complete)
          )
        )
      )

    implicit val responseCompleter: Completer[Response] =
      Completer.of(
        response => response.copy(
          value = response.value.complete
        )
      )

    Completer.of {
      record => 

        implicit val variants =
          record.getNgsReports.flatMap(_.variants)

        record.copy(
        patient = record.patient.complete,
        episodes = record.episodes.complete,
        diagnoses = record.diagnoses.complete,
        guidelineMedicationTherapies = record.guidelineMedicationTherapies.complete,
        guidelineProcedures = record.guidelineProcedures.complete,
        performanceStatus = record.performanceStatus.complete,
        specimens = record.specimens.complete,
        histologyReports = record.histologyReports.complete,
        ihcReports = record.ihcReports.complete,
        ngsReports = record.ngsReports.complete,
        carePlans = record.carePlans.complete, 
        medicationTherapies = record.medicationTherapies.map(
          _.map(
            th => th.copy(
              history = th.history.complete
            )
          )
        ),
        responses = record.responses.complete, 
      )

    }

  }


/*  
    implicit val Completer: Completer[] =
      Completer.of(
        .copy(
        )
      )

*/


  implicit protected val criteriaCompleter: Completer[MTBQueryCriteria] =
    Completer.of(
      criteria => criteria.copy(
        diagnoses = criteria.diagnoses.complete,
        tumorMorphologies = criteria.tumorMorphologies.map(
          _.map {
            coding =>
              implicit val cs =
                icdo3.morphology(coding.version.getOrElse(icdo3.latestVersion)).get
              coding.complete
          }
        ),
        responses = criteria.responses.complete,
      )
    )


  val CriteriaExpander: Completer[MTBQueryCriteria] = ???

}
