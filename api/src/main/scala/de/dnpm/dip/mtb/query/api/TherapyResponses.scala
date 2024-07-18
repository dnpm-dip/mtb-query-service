package de.dnpm.dip.mtb.query.api


import de.dnpm.dip.util.DisplayLabel
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.service.query.Distribution
import de.dnpm.dip.model.Medications
import de.dnpm.dip.mtb.model.{
  RECIST,
  Variant
}
import play.api.libs.json.{
  Json,
  OWrites
}



final case class TherapyResponseDistribution
(
  medicationClasses: Set[DisplayLabel[Coding[Medications]]],
  medications: Set[DisplayLabel[Coding[Medications]]],
  supportingVariants: Set[DisplayLabel[Variant]],
  responseDistribution: Distribution[Coding[RECIST.Value]]
)

object TherapyResponseDistribution
{
  implicit val writes: OWrites[TherapyResponseDistribution] =
    Json.writes[TherapyResponseDistribution]
}

/*
final case class TherapyResponseDistribution
(
  medicationClasses: Set[DisplayLabel[Coding[ATC]]],
  medications: Set[DisplayLabel[Coding[ATC]]],
  supportingVariants: Set[DisplayLabel[Variant]],
  responseDistribution: Distribution[Coding[RECIST.Value]]
)
*/
