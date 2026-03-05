package de.dnpm.dip.mtb.query.api


import play.api.libs.json.{
  Json,
  OWrites
}


final case class Ranked[T]
(
  resource: T,
  score: Double
)


object Ranked
{ 

  implicit def writes[T](
    implicit w: OWrites[T]
  ): OWrites[Ranked[T]] =
    OWrites { 
      ranked =>
        w.writes(ranked.resource) + ("score" -> Json.toJson(ranked.score))
    }

  implicit def ordering[T]: Ordering[Ranked[T]] =
    Ordering[Double]
      .on[Ranked[T]](_.score)
      .reverse  // Reverse, i.e. descending ordering to have entries with higher score first
}
