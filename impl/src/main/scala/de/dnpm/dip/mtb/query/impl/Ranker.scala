package de.dnpm.dip.mtb.query.impl


import scala.math.sqrt
import de.dnpm.dip.mtb.query.api.Ranked


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
