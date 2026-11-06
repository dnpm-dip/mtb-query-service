package de.dnpm.dip.mtb.query.impl



trait RelevanceMatcher[-T,V] extends Any
{

  def check(t: T): Seq[V]

  def score(t: T): Double

  def matches(t: T): Boolean 
}


trait BooleanRelevanceMatcher[-T] extends RelevanceMatcher[T,Boolean]
{

  def check(t: T): Seq[Boolean]

  override def score(t: T): Double = {
    val checks = check(t)
    checks.count(_ == true).toDouble/checks.size
  }

  override def matches(t: T): Boolean =
    check(t).forall(_ == true)

}
