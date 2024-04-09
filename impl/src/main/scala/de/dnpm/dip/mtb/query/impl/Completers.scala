package de.dnpm.dip.mtb.query.impl


import java.time.LocalDate
import cats.{
  Applicative,
  Id
}
import de.dnpm.dip.util.{
  Completer,
  DisplayLabel
}
import de.dnpm.dip.coding.{
  Code,
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
          managingSite = Some(Site.local)
        )
    )


  private implicit def hgvsCompleter[S <: HGVS]: Completer[Coding[S]] =
    Completer.of(
      coding => coding.copy(
        display = coding.display.orElse(Some(coding.code.value))
      )
    )


  private implicit val icdo3tCompleter: Completer[Coding[ICDO3.T]] =
    Completer.of {
      coding =>
        coding.version
          .flatMap(icdo3.topography(_))
          .getOrElse(icdo3.topography)
          .concept(coding.code)
          .map(
            concept =>
              coding.copy(
                display = Some(concept.display),
                version = concept.version
              )
          )
          .getOrElse(coding)
    }

  private implicit val icdo3mCompleter: Completer[Coding[ICDO3.M]] =
    Completer.of {
      coding =>
        coding.version
          .flatMap(icdo3.morphology(_))
          .getOrElse(icdo3.morphology)
          .concept(coding.code)
          .map(
            concept =>
              coding.copy(
                display = Some(concept.display),
                version = concept.version
              )
          )
          .getOrElse(coding)
    }




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
          code                     = diagnosis.code.complete, 
          whoGrading               = diagnosis.whoGrading.complete, 
          stageHistory             = diagnosis.stageHistory.map(st => st.copy(stage = st.stage.complete)), 
          guidelineTreatmentStatus = diagnosis.guidelineTreatmentStatus.complete,
          topography               = diagnosis.topography.complete
        )
      )


    implicit def indicationCompleter(
      implicit diagnoses: Seq[MTBDiagnosis]
    ): Completer[Reference[MTBDiagnosis]] =
      Completer.of(
        ref => ref.copy(
          display =
            ref.resolveOn(diagnoses)
              .map(_.code)
              .map(DisplayLabel.of(_).value)
        )
      )


    implicit def therapyCompleter(
      implicit diagnoses: Seq[MTBDiagnosis]
    ): Completer[MTBMedicationTherapy] =
      Completer.of {
        therapy =>
          therapy.copy(
            indication   = therapy.indication.complete,
            status       = therapy.status.complete,
            statusReason = therapy.statusReason.complete,
            medication   = therapy.medication.complete
          )
      }

    implicit def procedureCompleter(
      implicit diagnoses: Seq[MTBDiagnosis]
    ): Completer[OncoProcedure] =
      Completer.of {
        procedure => 
          procedure.copy(
            indication   = procedure.indication.complete,
            code         = procedure.code.complete,
            status       = procedure.status.complete,
            statusReason = procedure.statusReason.complete
        )
      }

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
            tumorMorphology  = report.results.tumorMorphology.map { obs => obs.copy(value = obs.value.complete) }
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
      implicit
      diagnoses: Seq[MTBDiagnosis],
      variants: List[Variant]
    ): Completer[MTBMedicationRecommendation] =
      Completer.of(
        recommendation =>
          recommendation.copy(
            indication      = recommendation.indication.complete,
            levelOfEvidence = recommendation.levelOfEvidence.map(
              loe => loe.copy(
                grading   = loe.grading.complete,
                addendums = loe.addendums.complete
              )
            ),
            priority        = recommendation.priority.complete,
            medication      = recommendation.medication.complete,
            supportingEvidence =
              recommendation.supportingEvidence
                .flatMap {
                  _.resolveOn(variants)
                   .map(
                     variant =>
                       Reference.to(variant)
                         .copy(display = Some(DisplayLabel.of(variant).value))
                   )
              }
          )
      )


    implicit def carePlanCompleter(
      implicit
      diagnoses: Seq[MTBDiagnosis],
      variants: List[Variant]
    ): Completer[MTBCarePlan] =
      Completer.of(
        carePlan => carePlan.copy(
          indication                      = carePlan.indication.complete,
          statusReason                    = carePlan.statusReason.complete,
          medicationRecommendations       = carePlan.medicationRecommendations.complete,
          geneticCounselingRecommendation = carePlan.geneticCounselingRecommendation.map(
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
  
        implicit val completedDiagnoses =
          record.diagnoses.complete.getOrElse(List.empty)


        record.copy(
        patient = record.patient.complete,
        episodes = record.episodes.complete,
        diagnoses = Option(completedDiagnoses),
        guidelineMedicationTherapies = record.guidelineMedicationTherapies.complete,
        guidelineProcedures = record.guidelineProcedures.complete,
        performanceStatus = record.performanceStatus.complete,
        specimens = record.specimens.complete,
        histologyReports = record.histologyReports.complete,
        ihcReports = record.ihcReports.complete,
        ngsReports = record.ngsReports.complete,
        carePlans = record.carePlans.complete, 
        medicationTherapies =
          record.medicationTherapies.map(
            _.map(
              th => th.copy(history = th.history.complete.sortBy(_.recordedOn)(Ordering[LocalDate].reverse))
            )
          ),
        responses = record.responses.complete, 
      )

    }

  }



  implicit val medicationCriteriaCompleter: Completer[MedicationCriteria] =
    Completer.of(
      med => med.copy(
        drugs = med.drugs.complete,
        usage = med.usage.complete
      )
    )

  implicit protected val criteriaCompleter: Completer[MTBQueryCriteria] = {

    implicit val snvCriteriaCompleter: Completer[SNVCriteria] = {

      val proteinChangeCompleter: Completer[Coding[HGVS]] =
        Completer.of {
          coding =>
            val threeLetterCode = HGVS.Protein.to3LetterCode(coding.code.value)
            coding.copy(
              code = Code[HGVS](threeLetterCode),
              display = coding.display.orElse(Some(threeLetterCode))
            )
        }

      Completer.of(
        snv => snv.copy(
          gene          = snv.gene.complete,
          dnaChange     = snv.dnaChange.complete,
          proteinChange = snv.proteinChange.map(proteinChangeCompleter)
        )
      )
    }

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
        tumorMorphologies = criteria.tumorMorphologies.complete,
        simpleVariants     = criteria.simpleVariants.complete,
        copyNumberVariants = criteria.copyNumberVariants.complete,
        dnaFusions         = criteria.dnaFusions.complete,
        rnaFusions         = criteria.rnaFusions.complete,
        medication         = criteria.medication.complete,
        responses          = criteria.responses.complete,
      )
    )

  }



  // By-name csp value (i.e. "lazy" as only evaluated upon being referenced) 
  // is required because the trait value is not yet initialized at this point,
  // resulting in weird null pointer exception
  def descendantExpander[T: Coding.System](
    implicit csp: => CodeSystemProvider[T,Id,Applicative[Id]]
  ): Completer[Set[Coding[T]]] =
    Completer.of(
      _.flatMap {
        coding =>
          val cs =
            coding.version
              .flatMap(csp.get)
              .getOrElse(csp.latest)

          (cs.concept(coding.code).toSet ++ cs.descendantsOf(coding.code))
            .map(_.toCoding)
      }
    )


  val CriteriaExpander: Completer[MTBQueryCriteria] = {

    implicit val icd10Expander: Completer[Set[Coding[ICD10GM]]] =
      descendantExpander[ICD10GM]

    implicit val atcExpander: Completer[Set[Coding[ATC]]] =
      descendantExpander[ATC]

    implicit val icdO3MExpander: Completer[Set[Coding[ICDO3.M]]] =
      Completer.of(
        _.flatMap(
          coding =>
            Set(coding.complete) ++
            coding.version
              .flatMap(icdo3.morphology(_))
              .getOrElse(icdo3.morphology)
              .descendantsOf(coding.code)
              .map(_.toCoding)
       )
     )


    Completer.of(
      criteria => criteria.copy(
        diagnoses         = criteria.diagnoses.complete,
        tumorMorphologies = criteria.tumorMorphologies.complete,
        medication        = criteria.medication.complete,
      )
    )

  }

}
