package de.dnpm.dip.mtb.query.api


import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.coding.icd.ICDO3
import de.dnpm.dip.coding.hgvs.HGVS
import de.dnpm.dip.model.{
  Gender,
  Interval,
  Site,
  Reference,
  Quantity
}
import de.dnpm.dip.service.query.{
  PatientFilter,
  Query,
  ResultSet,
  ConceptCount,
  Entry
}
import de.dnpm.dip.mtb.model.{
  MTBPatientRecord,
  Variant,
  RECIST
}
import play.api.libs.json.{
  Json,
  OWrites
}


final case class MTBResultSummary
(
  id: Query.Id,
  numPatients: Int,
  distributions: MTBResultSummary.Distributions,
) 
extends ResultSet.Summary
{
  type DistributionsType = MTBResultSummary.Distributions
}


object MTBResultSummary
{

  final case class Distributions
  (
    gender: Seq[ConceptCount[Coding[Gender.Value]]],
    age: Seq[ConceptCount[Interval[Int]]],
    site: Seq[ConceptCount[Coding[Site]]],
  )
  extends ResultSet.Distributions


  final case class TumorDiagnostics
  (
    entities: Seq[ConceptCount[Coding[ICD10GM]]],
    morphologies: Seq[ConceptCount[Coding[ICD10GM]]]
  )


  final case class Treatment
  (
    recommendationsTotal: Seq[ConceptCount[Coding[ATC]]],
    recommendationBySupportingVariant: Seq[Entry[Reference[Variant],Seq[ConceptCount[Coding[ATC]]]]],
    therapies: Seq[ConceptCount[Set[Coding[ATC]]]],
    meanTherapyDurations: Seq[Entry[Set[Coding[ATC]],Int]],
    responsesByTherapy: Seq[Entry[Set[Coding[ATC]],Seq[ConceptCount[RECIST.Value]]]]
  )


  implicit val writesDistributions: OWrites[Distributions] =
    Json.writes[Distributions]

  implicit val writes: OWrites[MTBResultSummary] =
    Json.writes[MTBResultSummary]
}


trait MTBResultSet
extends ResultSet[MTBPatientRecord,MTBQueryCriteria]
{
  type Summary = MTBResultSummary
}

