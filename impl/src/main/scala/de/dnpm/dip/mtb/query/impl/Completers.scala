package de.dnpm.dip.mtb.query.impl


import cats.{
  Applicative,
  Id
}
import de.dnpm.dip.util.{
  Completer,
  Tree
}
import de.dnpm.dip.coding.{
  Code,
  Coding,
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
  Medications
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


  private implicit val icdo3mCompleter: Completer[Coding[ICDO3.M]] =
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


  private implicit val medicationCriteriaCompleter: Completer[MedicationCriteria] =
    med => med.copy(
      items = med.items.complete,
      usage = med.usage.complete
    )


  protected implicit val proteinChangeCompleter: Completer[Code[HGVS.Protein]] =
    code => code.copy(
      value = HGVS.Protein.to3LetterCode(code.value)
    )    


  implicit protected val criteriaCompleter: Completer[MTBQueryCriteria] = {

    implicit val alterationCriteriaCompleter: Completer[GeneAlterationCriteria] =
      alteration => alteration.copy(
        gene = alteration.gene.complete,
        variant  = alteration.variant.collect {
          case snv: GeneAlterationCriteria.SNVCriteria =>
            snv.copy(
              proteinChange = snv.proteinChange.complete
            )              
          case cnv: GeneAlterationCriteria.CNVCriteria =>
            cnv.copy(
              copyNumberType = cnv.copyNumberType.complete
            )              
          case fusion: GeneAlterationCriteria.FusionCriteria =>
            fusion.copy(
              partner = fusion.partner.complete
            )
        }         
      )

    criteria => criteria.copy(
      diagnoses         = criteria.diagnoses.complete,
      tumorMorphologies = criteria.tumorMorphologies.complete,
      geneAlterations   = criteria.geneAlterations.map(
        obj => obj.copy(
          items = obj.items.complete
        )
      ),
      medication         = criteria.medication.complete,
      responses          = criteria.responses.complete,
    )

  }


  @deprecated("-","")
  val CriteriaExpander: Completer[MTBQueryCriteria] = {

    implicit val icd10Expander: Completer[Set[Coding[ICD10GM]]] =
      descendantExpander[ICD10GM]


    implicit val icdO3MExpander: Completer[Set[Coding[ICDO3.M]]] =
      _.flatMap(
         coding =>
           Set(coding.complete) ++
           coding.version
             .flatMap(icdo3.morphology(_))
             .getOrElse(icdo3.morphology)
             .descendantsOf(coding.code)
             .map(_.toCoding)
      )

    criteria => criteria.copy(
      diagnoses =
        criteria.diagnoses.complete,
      tumorMorphologies =
        criteria.tumorMorphologies.complete,
      medication =
        criteria.medication.map {
          _.complete
           .tap(mc =>
             mc.expandedDrugs =
               mc.items.flatMap {
                 coding => coding.system match {
                   case sys if sys == Coding.System[ATC].uri =>
                     coding.asInstanceOf[Coding[ATC]]
                      .expand
                      .map(_.asInstanceOf[Tree[Coding[Medications]]])

                   case _ => Some(Tree(coding.complete))
                 }
              }
          )
        }
    )

  }

}
