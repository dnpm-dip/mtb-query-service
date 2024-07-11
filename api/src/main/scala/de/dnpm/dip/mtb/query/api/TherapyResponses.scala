package de.dnpm.dip.mtb.query.api


import de.dnpm.dip.util.DisplayLabel
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.service.query.Distribution
import de.dnpm.dip.mtb.model.{
  RECIST,
  Variant
}
import play.api.libs.json.{
  Json,
  OWrites
}


final case class Medication
(
  therapyResponses: Seq[Medication.TherapyResponseDistribution]  
)


object Medication
{

  final case class TherapyResponseDistribution
  (
    medicationClasses: Set[DisplayLabel[Coding[ATC]]],
    medications: Set[DisplayLabel[Coding[ATC]]],
    supportingVariants: Set[DisplayLabel[Variant]],
    responseDistribution: Distribution[Coding[RECIST.Value]]
  )


  implicit val writesTherapyResponseDistribution: OWrites[TherapyResponseDistribution] =
    Json.writes[TherapyResponseDistribution]

  implicit val writes: OWrites[Medication] =
    Json.writes[Medication]

}
