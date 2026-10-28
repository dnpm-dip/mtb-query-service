package de.dnpm.dip.mtb.query.impl


import cats.Applicative
import cats.data.NonEmptyList
import de.dnpm.dip.coding.{
  CodeSystemProvider,
  Coding
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.{
  ICD10GM,
  ICDO3
}
import de.dnpm.dip.model.{
  Id,
  Medications,
  Site
}
import de.dnpm.dip.mtb.model.{
  MTBPatientRecord
}
import de.dnpm.dip.service.Distribution
import de.dnpm.dip.mtb.query.api.FeasibilityQuery
import FeasibilityQuery._
import de.dnpm.dip.model.Medications._  // For extensions methods on Coding[Medications]


object FeasibilityQueryOps extends MTBReportingOps
{

  implicit class LocalResultsOps(val obj: LocalResults.type) extends AnyVal
  {

    def of(
      site: Coding[Site],
      records: Seq[MTBPatientRecord]
    )(
      implicit
      atc: CodeSystemProvider[ATC,cats.Id,Applicative[cats.Id]],
      icd10gm: CodeSystemProvider[ICD10GM,cats.Id,Applicative[cats.Id]],
      icdo3: CodeSystemProvider[ICDO3,cats.Id,Applicative[cats.Id]]
    ): LocalResults = {
  
      val diagnostics =
        overallDiagnosticDistributions(records)
  
      val latestTherapies =
        records.flatMap(_.getSystemicTherapies.map(_.latest))
  
      LocalResults(
        site = site,
        cohortSize = records.size,
        gender = Distribution.of(records.map(_.patient.gender)),
        age = Distribution.of(records.map(_.patient.age), step = 10),
        vitalStatus = Distribution.of(records.map(_.patient.vitalStatus)),
        tumorEntities = diagnostics.tumorEntities,
        tumorMorphologies = diagnostics.tumorMorphologies,
        recommendedMedication = recommendationDistribution(records),
        therapyStatus = Distribution.of(latestTherapies.map(_.status)),
        therapyStatusReason = Distribution.of(latestTherapies.flatMap(_.statusReason)),
        ecogStatus = Distribution.of(records.flatMap(_.performanceStatus.flatMap(_.maxByOption(_.effectiveDate).map(_.value)))),
        usedMedication = Distribution.byParent(
          latestTherapies.flatMap(_.medication),
          (meds: Set[Coding[Medications]]) => meds.map(coding => coding.currentGroup.getOrElse(coding)),
        )
      )
    }

  }


  implicit class AggregatedResultsOps(val obj: AggregatedResults.type) extends AnyVal
  {

    def of(
      query: Id[FeasibilityQuery],
      localResults: NonEmptyList[LocalResults],
      cutoff: Int
    ): Option[AggregatedResults] = {
 
      val cohortSize = localResults.map(_.cohortSize).toList.sum
 
      Option.when(cohortSize >= cutoff)(
        AggregatedResults(
          query,
          localResults.map(_.site),
          cohortSize,
          localResults.map(_.gender).reduce,
          localResults.map(_.age).reduce,
          localResults.map(_.vitalStatus).reduce,
          localResults.map(_.tumorEntities).reduce,
          localResults.map(_.tumorMorphologies).reduce,
          localResults.map(_.recommendedMedication).reduce,
          localResults.map(_.therapyStatus).reduce,
          localResults.map(_.therapyStatusReason).reduce,
          localResults.map(_.ecogStatus).reduce,
          localResults.map(_.usedMedication).reduce,
        )
      )
 
    }

  }

}

