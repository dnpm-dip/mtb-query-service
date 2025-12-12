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


  implicit class TherapyResponseRanker(val criteria: MTBQueryCriteria) extends RelevanceMatcher[RankableTherapyResponses,Double] 
  {

    import GeneAlterationExtensions._

    // Euclidian norm of the vector represented as Seq[Double]
    private def norm(vec: Seq[Double]): Double =
      math.sqrt(vec.map(e => e*e).sum)


    // Sequence of element-wise product of query and "document" vector,
    // as preliminary stage for vector space ranking
    override def check(th: RankableTherapyResponses): Seq[Double] = {
      Seq(
        criteria.diagnoses.map(_ exists (_.code == th.entity.code)),
        criteria.geneAlterations.map(_.items exists (_ matches th.supportingAlteration)),
        // TODO: criteria.medication 
        criteria.responses.map(_ exists (recist => th.responseDistribution.elements.exists(_.key == recist.code.enumValue)))
      )
      .collect { 
        case Some(true) => 1.0
        case _          => 0.0
      }
    }

    // Vector space model ranking, i.e. weighted dot product of query and "document" vector
    override def score(th: RankableTherapyResponses): Double = {

      // Convert "criteria" into a boolean query vector, i.e. with 1.0 values for each occurring term
      val queryVector =
        Seq(
          criteria.diagnoses.map(_.size),
          criteria.geneAlterations.map(_.items.size),
//          criteria.medications.map(_.items.size),
          criteria.responses.map(_.size),
        )
        .map(_.map(Seq.fill(_)(1.0)).getOrElse(Seq.empty))
        .flatten

      if (queryVector.isEmpty)
        0.0
      else {
        // Convert "th" into a boolean document vector, i.e. with 1.0 values for each occurring term
        val docVector =
          Seq.fill(th.medications.size)(1.0) :++ Seq.fill(th.responseDistribution.elements.size)(1.0) :+ 1.0 :+ 1.0

        check(th).sum/(norm(queryVector) * norm(docVector))
      }

    }


    override def matches(t: RankableTherapyResponses): Boolean = 
      score(t) > 0.0

  }

}
