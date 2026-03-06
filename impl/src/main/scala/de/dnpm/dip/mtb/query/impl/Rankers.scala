package de.dnpm.dip.mtb.query.impl


import de.dnpm.dip.mtb.query.api.MTBQueryCriteria
import de.dnpm.dip.mtb.query.api.MTBResultSet.{
  GeneAlterationInfo,
  TherapyResponses
}


trait Rankers
{

  /**
   * Convert MTBQueryCriteria into a "bag of words" representation for relevance ranking
   * @param criteria Structured query criteria
   * @return "bag of words" representation of query criteria
   */
  implicit def queryVector(criteria: MTBQueryCriteria): Set[Any] =
    criteria.diagnoses.getOrElse(Set.empty).map(_.code) ++
    criteria.geneAlterations.map(_.items).getOrElse(Set.empty).map(_.gene.code) ++
    criteria.medication.map(_.items).getOrElse(Set.empty).flatMap(_.display.map(_.toLowerCase)) ++
    criteria.responses.getOrElse(Set.empty).map(_.code.enumValue)


  /**
   * Converter of GeneAlterationInfo object into a "bag of words" representation for relevance ranking
   */
  lazy val GeneAlterationInfoRanker =
    Ranker.of[GeneAlterationInfo,Any]{
      case GeneAlterationInfo(entity,gene,alteration,_,_) =>
        Set(
          entity.code,
          gene.code,
          alteration
        )
    }

  /**
   * Converter of TherapyResponses object into a "bag of words" representation for relevance ranking
   */
  lazy val TherapyResponsesRanker =
    Ranker.of[TherapyResponses,Any]{
      th =>
        Set(
          th.entity.code,
          th.supportingAlteration.gene.code
        ) ++
        th.medications.flatMap(_.display.map(_.toLowerCase)) ++
        th.responseDistribution.elements.map(_.key)
    }
}

object Rankers extends Rankers
