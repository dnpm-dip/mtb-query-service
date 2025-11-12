package de.dnpm.dip.mtb.query.impl


import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.model.Medications
import de.dnpm.dip.mtb.model.RECIST
import de.dnpm.dip.mtb.query.api.{
  GeneAlteration,
  MTBQueryCriteria
}
import de.dnpm.dip.service.Distribution


final case class RankableTherapyResponses
(
  entity: Coding[ICD10GM],
  medications: Set[Coding[Medications]],
  supportingAlteration: GeneAlteration,
  count: Int,
  orr: Int,  // Overall Response Rate: 0 - 100 %
  responseDistribution: Distribution[RECIST.Value],
  meanDuration: Double,
)


object TherapyResponseRanking
{

/*  
final case class MTBQueryCriteria
(
  diagnoses: Option[Set[Coding[ICD10GM]]],
  tumorMorphologies: Option[Set[Coding[ICDO3.M]]],
  geneAlterations: Option[GeneAlterations],
  medication: Option[MedicationCriteria],
  responses: Option[Set[Coding[RECIST.Value]]]
)
*/

  implicit class TherapyResponseRanker(
    val criteria: MTBQueryCriteria
  )
  extends RelevanceMatcher[RankableTherapyResponses,Double] 
  {

    import GeneAlterationExtensions._

    override def check(th: RankableTherapyResponses): Seq[Double] = {
      Seq(
        criteria.diagnoses.map(_ exists (_.code == th.entity.code)).getOrElse(true),
        criteria.geneAlterations.map(_.items.exists(_ matches th.supportingAlteration)).getOrElse(true),
        // TODO: criteria.medication 
      )
      .collect { 
        case true  => 1.0
        case false => 0.0
      }
    }

    // Basic relevance ranking: Euclidian length of feature vector as score
    override def score(t: RankableTherapyResponses): Double =
      math.sqrt(check(t).map(e => e*e).sum)

    override def matches(t: RankableTherapyResponses): Boolean = 
      score(t) > 0.0
  }

}

