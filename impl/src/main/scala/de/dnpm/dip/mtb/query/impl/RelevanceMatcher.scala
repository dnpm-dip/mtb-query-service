package de.dnpm.dip.mtb.query.impl


trait RelevanceMatcher[-T] extends Any {

  def check(t: T): Seq[Boolean]

  def score(t: T): Double = {
    val checks = check(t)
    checks.count(_ == true).toDouble/checks.size
  }

  def matches(t: T): Boolean =
    check(t).forall(_ == true)
}
