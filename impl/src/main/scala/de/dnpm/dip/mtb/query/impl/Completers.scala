package de.dnpm.dip.mtb.query.impl


import java.time.LocalDate
import cats.{
  Applicative,
  Id
}
import de.dnpm.dip.util.{
  Completer,
  DisplayLabel,
  Tree
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
  BaseCompleters,
  Medications,
  Patient,
  Site,
  Reference,
  Resolver
}
import de.dnpm.dip.mtb.query.api._


trait Completers extends BaseCompleters
{

  import scala.util.chaining._
  import Completer.syntax._


  implicit val hgnc: CodeSystemProvider[HGNC,Id,Applicative[Id]]

  implicit val atc: CodeSystemProvider[ATC,Id,Applicative[Id]]

  implicit val icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]]

  implicit val icdo3: ICDO3.Catalogs[Id,Applicative[Id]]



  private implicit def treeCompleter[T: Completer]: Completer[Tree[T]] =
    Completer.of(
      t => t.copy(
        element = t.element.complete
      )
    )


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

  implicit protected val criteriaCompleter: Completer[MTBQueryCriteria] = {

    implicit val snvCriteriaCompleter: Completer[SNVCriteria] = {

      val proteinChangeCompleter: Completer[Coding[HGVS.Protein]] =
        Completer.of {
          coding =>
            val threeLetterCode = HGVS.Protein.to3LetterCode(coding.code.value)
            coding.copy(
              code = Code[HGVS.Protein](threeLetterCode),
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

    implicit val medicationCriteriaCompleter: Completer[MedicationCriteria] =
      Completer.of(
        med => med.copy(
          drugs = med.drugs.complete,
          usage = med.usage.complete
        )
      )

    Completer.of(
      criteria => criteria.copy(
        diagnoses         = criteria.diagnoses.complete,
        tumorMorphologies = criteria.tumorMorphologies.complete,
        variants           = criteria.variants.map(
          vs => vs.copy(
            simpleVariants     = vs.simpleVariants.complete,
            copyNumberVariants = vs.copyNumberVariants.complete,
            dnaFusions         = vs.dnaFusions.complete,
            rnaFusions         = vs.rnaFusions.complete,
          )
        ),
        medication         = criteria.medication.complete,
        responses          = criteria.responses.complete,
      )
    )

  }



  val CriteriaExpander: Completer[MTBQueryCriteria] = {

    implicit val icd10Expander: Completer[Set[Coding[ICD10GM]]] =
      descendantExpander[ICD10GM]


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
        diagnoses =
          criteria.diagnoses.complete,
        tumorMorphologies =
          criteria.tumorMorphologies.complete,
        medication =
          criteria.medication.map(
            med => med.copy(
              drugs = med.drugs.flatMap {
                t => t.element.system match {
                  case sys if sys == Coding.System[ATC].uri =>
                    t.element
                     .asInstanceOf[Coding[ATC]]
                     .expand
                     .map(_.asInstanceOf[Tree[Coding[Medications]]])

                  case _ => Some(Tree(t.element.complete))
                }
              }
//              drugs = med.drugs.flatMap(_.element.expand)
            )
          )
      )
    )

  }

}
