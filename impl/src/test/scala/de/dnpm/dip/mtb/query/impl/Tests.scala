package de.dnpm.dip.mtb.query.impl


import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.OptionValues._
import org.scalatest.EitherValues._
import org.scalatest.Inspectors._
import scala.util.Random
import scala.concurrent.Future
import de.dnpm.dip.model.{
  Gender,
  Site
}
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.mtb.query.api._
import de.dnpm.dip.mtb.model.MTBPatientRecord
import de.dnpm.dip.mtb.model.Completers._
import de.dnpm.dip.service.query.{
  Query,
  Querier,
  PreparedQuery,
}
import de.dnpm.dip.service.query.PatientFilter
import de.dnpm.dip.service.query.QueryService.Save
import de.dnpm.dip.connector.HttpConnector
import de.ekut.tbi.generators.Gen


class Tests extends AsyncFlatSpec
{

  import de.dnpm.dip.util.Completer.syntax._
  import de.dnpm.dip.mtb.gens.Generators._

  System.setProperty(Site.property,"UKx:Musterlingen")
  System.setProperty(HttpConnector.Type.property,"fake")
  System.setProperty(MTBLocalDB.dataGenProp,"0")


  implicit val rnd: Random =
    new Random

  implicit val querier: Querier =
    Querier("Dummy-Querier-ID")

 
  val serviceTry =
    MTBQueryService.getInstance

  lazy val service = serviceTry.get


  val dataSets =
    LazyList.fill(50)(Gen.of[MTBPatientRecord].next)
      .map(_.complete)


  // Generator for non-empty Query Criteria based on features occurring in a given dataset,
  // and thus guaranteed to always match at least this one data set
  val genCriteria: Gen[MTBQueryCriteria] =
    for {
      patRec <- Gen.oneOf(dataSets)

      icd10 = patRec.diagnoses.head.code

      ngs = 
        patRec
          .getNgsReports.head

      snvCriteria =
        ngs.results
          .simpleVariants
          .take(2)
          .map(snv =>
            SNVCriteria(
              Some(snv.gene),
              None,
              snv.proteinChange
                // Change the protein change to just a substring of the occurring one
                // to test that matches are also returned by substring match of the protein (or DNA) change
                .map(
                  pch => pch.copy( 
                    value = pch.value.substring(2,pch.value.size-1)
                  )
                )
            )
          )
          .toSet
/*
      cnv = 
        ngs.results
          .copyNumberVariants
          .head

      cnvCriteria =
        CNVCriteria(
          Some(cnv.reportedAffectedGenes.getOrElse(Set.empty).take(1)),
          Some(cnv.`type`)
        )
*/
      medicationCriteria =
        patRec
          .getSystemicTherapies
          .map(_.latest)
          .collectFirst {
            case th if th.medication.isDefined =>
              MedicationCriteria(
                Some(LogicalOperator.And),
                th.medication
                  .get
                  .map(_.asInstanceOf[Coding[ATC]]),
                Some(Set(Coding(MedicationUsage.Used)))
             )
          }

    } yield MTBQueryCriteria(
      Some(Set(icd10)),
      None,
      Some(
        GeneAlterations(
          Some(LogicalOperator.Or),
          snvCriteria.map(
            crit =>
            GeneAlterationCriteria(
              crit.gene.get,
              Some(
                GeneAlterationCriteria.SNVCriteria(
                  crit.dnaChange,
                  crit.proteinChange
                )
              )
            )
          )
        )
      ),
      None,
/*      
      Some(
        VariantCriteria(
          Some(LogicalOperator.Or),
          Some(snvCriteria),
          Some(Set(cnvCriteria)),
          None,
          None
        )
      ),
*/    
      medicationCriteria,
      None
    )



  "SPI" must "have worked" in {
    serviceTry.isSuccess mustBe true
  }


  "Importing MTBPatientRecords" must "have worked" in {

    for {
      outcomes <- Future.traverse(dataSets)(service ! Save(_))
    } yield all (outcomes.map(_.isRight)) mustBe true 
    
  }


  val queryMode =
    Coding(Query.Mode.Local)


  "Query ResultSet" must "contain the total number of data sets for a query without criteria" in {

    for {
      result <-
        service ! Query.Submit(
          queryMode,
          None,
          None
        )

      query = result.value

      resultSet <- service.resultSet(query.id).map(_.value)

    } yield resultSet.demographics(MTBFilters.empty).patientCount must equal (dataSets.size)  

  }


  it must "contain a non-empty list of correctly matching data sets for a query with criteria" in {

    import MTBQueryCriteriaOps._

    for {
      result <-
        service ! Query.Submit(
          queryMode,
          None,
          Some(genCriteria.next)
        )

      query = result.value

      queryCriteria =
        query.criteria.value

      resultSet <-
        service.resultSet(query.id)
          .map(_.value)

      patientMatches = 
        resultSet.patientMatches(MTBFilters.empty)

      _ = patientMatches must not be empty

      _ = patientMatches.size must be < (dataSets.size) 

      matchingCriteria =
        patientMatches.map(_.matchingCriteria)

      _ = all (matchingCriteria) must be (defined)

    } yield forAll(matchingCriteria){ 
      matches =>
        assert( (queryCriteria intersect matches.value).nonEmpty )
    }

  }


  "Filtering" must "have worked" in {

    for {
      result <-
        service ! Query.Submit(
          queryMode,
          None,
          None
        )

      query = result.value

      resultSet <-
        service.resultSet(query.id)
          .map(_.value)

      filter =
        MTBFilters.empty
          .copy(
            patient = PatientFilter.empty.copy(
              gender = Some(Set(Coding(Gender.Female)))
            )
          )
      patientMatches = 
        resultSet.patientMatches(filter)

    } yield patientMatches.size must be < (dataSets.size)

  }


  "PreparedQuery" must "have been successfully created" in {

    for {
      result <-
        service ! PreparedQuery.Create("Dummy Prepared Query",genCriteria.next)

    } yield result.isRight mustBe true 

  }

  it must "have been successfully retrieved" in {

    for {
      result <-
        service ? PreparedQuery.Filter(Some(querier))

      _ = result must not be empty 

      query <- 
        service ? result.head.id

    } yield query must be (defined)

  }

}
