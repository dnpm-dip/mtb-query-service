package de.dnpm.dip.mtb.query.impl


import de.dnpm.dip.mtb.query.api.{
  GeneAlteration,
  GeneAlterations,
  MTBQueryCriteria
}
import de.dnpm.dip.mtb.query.api.MTBResultSet.{
  GeneAlterationInfo,
  TherapyResponses
}
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.icd.ICD10GM


trait Rankers
{

  implicit def toQueryVector(geneAlterations: GeneAlterations): Set[Any] =
    geneAlterations.items.map(_.gene.code)


//  implicit val icd10gm: CodeSystemProvider[ICD10GM,cats.Id,Applicative[cats.Id]]


  /**
   * Convert MTBQueryCriteria into a "bag of words" representation for relevance ranking
   * @param criteria Structured query criteria
   * @return "bag of words" representation of query criteria
   */
  implicit def toQueryVector(criteria: MTBQueryCriteria): Set[Any] =
    criteria.tumorEntities.getOrElse(Set.empty).map(_.code) ++
    criteria.geneAlterations.map(toQueryVector).getOrElse(Set.empty) ++
    criteria.medication.map(_.items).getOrElse(Set.empty).flatMap(_.display.map(_.toLowerCase)) ++
    criteria.responses.getOrElse(Set.empty).map(_.code.enumValue)


  lazy val GeneAlterationRanker =
    Ranker.of[GeneAlteration,Any]{
      case GeneAlteration.SNV(gene,proteinChange) => Set(gene.code) ++ proteinChange
        
      case GeneAlteration.CNV(gene,copyNumberType) => Set(gene.code,copyNumberType)

      case GeneAlteration.Fusion(gene,partner) => Set(gene.code,partner.code)

      case GeneAlteration.Unspecified(gene) => Set(gene.code)
    }


  /**
   * Converter of GeneAlterationInfo object into a "bag of words" representation for relevance ranking
   *
   * NOTE: The associated Set[Coding[ICD10GM]] contains the original ICD-10 codes
   * which occurred, but were resolved to the parent category code in GeneAlterationInfo,
   * e.g. GeneAlterationInfo(C71,...), Set(C71.1, C71.2)
   */
  lazy val GeneAlterationInfoRanker =
    Ranker.of[(GeneAlterationInfo,Set[Coding[ICD10GM]]),Any]{
      case GeneAlterationInfo(entity,alteration,_,_) -> entities =>
        entities.map[Any](_.code) + entity.code + alteration.gene.code
    }


  /**
   * Converter of TherapyResponses object into a "bag of words" representation for relevance ranking
   */
  lazy val TherapyResponsesRanker =
    Ranker.of[TherapyResponses,Any]{
      th =>
        Set(
          th.tumorEntity.code,
          th.supportingAlteration.gene.code
        ) ++
        th.medications.flatMap(_.display.map(_.toLowerCase)) ++
        th.responseDistribution.elements.map(_.key)
    }


}

object Rankers extends Rankers
