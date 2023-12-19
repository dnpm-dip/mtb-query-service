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
import de.dnpm.dip.coding.hgvs.HGVS
import de.dnpm.dip.model.{
  Patient,
  Site,
  Reference,
  Resolver,
}
import de.dnpm.dip.mtb.model._
import de.dnpm.dip.mtb.query.api._



trait Completers
{

  import scala.util.chaining._
  import Completer.syntax._
  import MTBMedicationTherapy.statusReasonCodeSystem


  val localSite: Coding[Site]

  implicit val hgnc: CodeSystem[HGNC]

  implicit val atc: CodeSystemProvider[ATC,Id,Applicative[Id]]

  implicit val icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]]

  implicit val icdo3: ICDO3.Catalogs[Id,Applicative[Id]]

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


  private implicit def hgvsCompleter[S <: HGVS]: Completer[Coding[S]] =
    Completer.of(
      coding => coding.copy(
        display = coding.display.orElse(Some(coding.code.value))
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
            diagnosis.topography.map {
              coding =>
                implicit val cs =
                  icdo3.topography(coding.version.getOrElse(icdo3.latestVersion)).get
                coding.complete
            }
      
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
              report.results.tumorMorphology.map {
                obs =>
                  implicit val cs =
                    icdo3.morphology(obs.value.version.getOrElse(icdo3.latestVersion)).get

                  obs.copy(value = obs.value.complete)
              }
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
          chromosome     = snv.chromosome.complete,
          gene           = snv.gene.complete,
          dnaChange      = snv.dnaChange.complete,
          proteinChange  = snv.proteinChange.complete,
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



  implicit val medicationCriteriaCompleter: Completer[MedicationCriteria] =
    Completer.of(
      med => med.copy(
        medication = med.medication.complete,
        usage      = med.usage.complete
      )
    )

  implicit protected val criteriaCompleter: Completer[MTBQueryCriteria] = {

    implicit val snvCriteriaCompleter: Completer[SNVCriteria] =
      Completer.of(
        snv => snv.copy(
          gene          = snv.gene.complete,
          dnaChange     = snv.dnaChange.complete,
          proteinChange = snv.proteinChange.complete
        )
      )

    implicit val cnvCriteriaCompleter: Completer[CNVCriteria] =
      Completer.of(
        cnv => cnv.copy(
          affectedGenes = cnv.affectedGenes.complete,
          `type`        = cnv.`type`.complete
        )
      )

    implicit val fusionCriteriaCompleter: Completer[FusionCriteria] =
      Completer.of(
        fusion => fusion.copy(
          fusionPartner5pr = fusion.fusionPartner5pr.complete,
          fusionPartner3pr = fusion.fusionPartner3pr.complete,
        )
      )

    Completer.of(
      criteria => criteria.copy(
        diagnoses         = criteria.diagnoses.complete,
        tumorMorphologies = criteria.tumorMorphologies.map(
          _.map {
            coding =>
              implicit val cs =
                icdo3.morphology(coding.version.getOrElse(icdo3.latestVersion)).get
              coding.complete
          }
        ),
        simpleVariants     = criteria.simpleVariants.complete,
        copyNumberVariants = criteria.copyNumberVariants.complete,
        dnaFusions         = criteria.dnaFusions.complete,
        rnaFusions         = criteria.rnaFusions.complete,
        medications        = criteria.medications.complete,
        responses          = criteria.responses.complete,
      )
    )

  }


  val CriteriaExpander: Completer[MTBQueryCriteria] = {

    implicit val icd10Expander: Completer[Set[Coding[ICD10GM]]] =
      Completer.of(
        _.flatMap(
          coding =>
            Set(coding.complete) ++
            CodeSystemProvider[ICD10GM]
              .pipe(
                csp =>
                  csp.get(coding.version.getOrElse(csp.latestVersion))
                   .get
                   .descendantsOf(coding.code)
                   .map(_.toCoding)
            )
        )
      )

    implicit val atcExpander: Completer[Set[Coding[ATC]]] =
      Completer.of(
        _.flatMap(
          coding =>
            Set(coding.complete) ++
            CodeSystemProvider[ATC]
              .pipe(
                csp =>
                  csp.get(coding.version.getOrElse(csp.latestVersion))
                   .get
                   .descendantsOf(coding.code)
                   .map(_.toCoding)
            )
        )
    )

    implicit val icdO3MExpander: Completer[Set[Coding[ICDO3.M]]] =
      Completer.of(
        _.flatMap {
          coding =>
            implicit val cs =
              icdo3.morphology(coding.version.getOrElse(icdo3.latestVersion)).get

            Set(coding.complete) ++
              CodeSystem[ICDO3.M]
                .descendantsOf(coding.code)
                .map(_.toCoding)
        }
      )


    Completer.of(
      criteria => criteria.copy(
        diagnoses         = criteria.diagnoses.complete,
        tumorMorphologies = criteria.tumorMorphologies.complete,
        medications       = criteria.medications.complete,
      )
    )


  }

}
