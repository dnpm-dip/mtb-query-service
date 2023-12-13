package de.dnpm.dip.mtb.query.api


import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.hgvs.HGVS
import de.dnpm.dip.model.{
  Gender,
  Interval,
  Site,
  Reference,
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
  Variant
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
  therapeutics:
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


  final case class Therapeutics
  (
    recommendationsTotal: Seq[ConceptCount[Coding[ATC]]],
    recommendationBySupportingVariant: Seq[Entry[Reference[Variant],Seq[ConceptCount[Coding[ATC]]]]],
    therapies: Seq[ConceptCount[Coding[ATC]]]
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

