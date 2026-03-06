package de.dnpm.dip.mtb.query.impl


import scala.math.sqrt
import de.dnpm.dip.mtb.query.api.Ranked


/**
 * Abstraction to compute a relevance score (a.k.a retrieval status value)
 * for relevance ranking of T instances
 */
trait Ranker[T]
{
  def rank(t: T): Double
}


object Ranker
{ 

  def unranked[T]: Ranker[T] =
    new Ranker[T]{
      override def rank(t: T) = 1.0
    }


  /**
   * Basic Ranker implementation based on Vector Space Model (https://en.wikipedia.org/wiki/Vector_space_model):
   *
   * For a given "query vector" q and "document vector" d, the relevance score is computed 
   * as the cosine of the angle between q and d, i.e. cos(theta) = (q dot d)/norm(q)*norm(d)
   *
   * VectorSpaceRanker is parameterized on the type of "Term" and "Doc(ument)".
   * q and d are here represented as "bag of words", i.e. Set[Term]. 
   * Taking this to be equivalent to a sparse vector representation of boolean values (0,1) for each dimension/term,
   * the cosine computation thus becomes equivalent to:
   * (q intersect d)/sqrt(q.size)*sqrt(d.size)
   *
   * @param queryVector "bag of words" representation of query vector
   * @param docVector Function to map a Doc to a "bag of words" representation
   *
   */
  private class VectorSpaceRanker[Term,Doc](
    queryVector: Set[Term],
    docVector: Doc => Set[Term]
  )
  extends Ranker[Doc]
  {
    override def rank(doc: Doc): Double = { 

      val document = docVector(doc)

      (queryVector & document).size.toDouble/(sqrt(queryVector.size.toDouble) * sqrt(document.size.toDouble))

    }
  }

  /**
   * Partially applied function to serve as Builder of VectorSpaceRanker[Term,Doc]
   *
   * @param docVector Function to map a Doc to a "bag of words" representation
   */
  def of[Doc,Term](docVector: Doc => Set[Term]): Set[Term] => Ranker[Doc] =
    new VectorSpaceRanker[Term,Doc](_,docVector)


  object syntax
  {

    implicit class RankingOps[T](val t: T) extends AnyVal
    { 

      def ranked(implicit ranker: Ranker[T]) =
        Ranked(t,ranker.rank(t))

      def rankedBy[U](u: T => U)(implicit ranker: Ranker[U]) =
        Ranked(t,ranker.rank(u(t)))

    }

  }

}
