package de.dnpm.dip.mtb.query.api


import cats.{
  Id,
  Applicative
}
import de.dnpm.dip.util.DisplayLabel
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.coding.icd.ICDO3
import de.dnpm.dip.model.Medications
import de.dnpm.dip.service.{
  Entry,
  Distribution
}
import de.dnpm.dip.service.query.ResultSet
import de.dnpm.dip.mtb.model.{
  MTBPatientRecord,
  Variant,
  RECIST
}
import play.api.libs.json.{
  Json,
  OWrites
}


trait MTBResultSet
extends ResultSet[MTBPatientRecord,MTBQueryCriteria]
with KaplanMeierOps[Id,Applicative[Id]]
{

  type Filter = MTBFilters


  def tumorDiagnostics(
    filter: MTBFilters
  ): MTBResultSet.TumorDiagnostics

  def medication(
    filter: MTBFilters
  ): MTBResultSet.Medication

  def therapyResponses(
    filter: MTBFilters
  ): Seq[MTBResultSet.TherapyResponseDistribution]

  def therapyResponsesBySupportingVariant(
    filter: MTBFilters
  ): Seq[MTBResultSet.TherapyResponses]

  def alterationsByGene(
    filter: MTBFilters
  ): MTBResultSet.AlterationDistributions

}


object MTBResultSet
{

  object TumorDiagnostics
  { 

    final case class Distributions
    (
      tumorEntities: Distribution[Coding[ICD10GM]],
      tumorMorphologies: Distribution[Coding[ICDO3]]
    )


    implicit val writesDistributions: OWrites[Distributions] =
      Json.writes[Distributions]
  }


  final case class TumorDiagnostics
  (
    overallDistributions: TumorDiagnostics.Distributions,
    distributionsByVariant: Seq[Entry[DisplayLabel[GeneAlteration],TumorDiagnostics.Distributions]]
  )


  final case class AlterationDistributions
  (
    alterationsByGene: Seq[Entry[Coding[HGNC],Distribution[GeneAlteration]]]
  )


  final case class Medication
  (
    recommendations: Medication.Recommendations,
    therapies: Medication.Therapies
  )
  object Medication
  {

    final case class Recommendations
    (
      overallDistribution: Distribution[Set[Coding[Medications]]],
      distributionBySupportingVariant: Seq[Entry[DisplayLabel[GeneAlteration],Distribution[Set[Coding[Medications]]]]]
    )

    final case class Therapies
    (
      overallDistribution: Distribution[Set[Coding[Medications]]],
      meanDurations: Seq[Entry[Set[Coding[Medications]],Double]]
    )


    implicit val writesRecommendations: OWrites[Recommendations] =
      Json.writes[Recommendations]

    implicit val writesTherapies: OWrites[Therapies] =
      Json.writes[Therapies]

    implicit val writes: OWrites[Medication] =
      Json.writes[Medication]

  }


  final case class TherapyResponseDistribution
  (
    medicationClasses: Set[Coding[Medications]],
    medications: Set[Coding[Medications]],
    supportingVariants: Set[DisplayLabel[Variant]],
    responseDistribution: Distribution[Coding[RECIST.Value]]
  )

  object TherapyResponseDistribution
  {
    implicit val writes: OWrites[TherapyResponseDistribution] =
      Json.writes[TherapyResponseDistribution]
  }


  final case class TherapyResponses
  (
    supportingVariant: DisplayLabel[Variant],
    medicationClasses: Set[Coding[Medications]],
    medications: Set[Coding[Medications]],
    responseDistribution: Distribution[Coding[RECIST.Value]]
  )

  object TherapyResponses
  {
    implicit val writes: OWrites[TherapyResponses] =
      Json.writes[TherapyResponses]
  }


  implicit val writesTumorDiagnostics: OWrites[TumorDiagnostics] =
    Json.writes[TumorDiagnostics]

}
