package de.dnpm.dip.mtb.query.impl


import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.OptionValues._
import org.scalatest.EitherValues._
import org.scalatest.Inspectors._
import scala.util.Random
import scala.concurrent.Future
import cats.Monad
import de.dnpm.dip.util.Tree
import de.dnpm.dip.model.Site
import de.dnpm.dip.coding.{
  Code,
  CodeSystem,
  Coding
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.hgvs.HGVS
import de.dnpm.dip.mtb.query.api._
import de.dnpm.dip.mtb.model.MTBPatientRecord
import de.dnpm.dip.mtb.model.Completers._
import de.dnpm.dip.service.Data.Save
import de.dnpm.dip.service.query.{
  BaseQueryCache,
  Query,
  Querier,
  PreparedQuery,
  PreparedQueryDB,
  InMemPreparedQueryDB
}
import de.dnpm.dip.connector.{
  HttpConnector,
  FakeConnector
}
import de.ekut.tbi.generators.Gen
import play.api.libs.json.{
  Json,
  Writes
}


class Tests extends AsyncFlatSpec
{

  import scala.util.chaining._
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

      icd10 =
        patRec.getDiagnoses
          .head
          .code

      ngs = 
        patRec
          .getNgsReports.head

      snv =
        ngs.results
          .simpleVariants
          .head

      snvCriteria =
        SNVCriteria(
          snv.gene,
          None,
          snv.proteinChange
            // Change the protein change to just a substring of the occurring one
            // to test that matches are also returned by substring match of the protein (or DNA) change
            .map(
              pch => pch.copy( 
                code = Code[HGVS.Protein](pch.code.value.substring(2,pch.code.value.size-1))
              )
            )
        )

      cnv = 
        ngs.results
          .copyNumberVariants
          .head

      cnvCriteria =
        CNVCriteria(
          Some(cnv.reportedAffectedGenes.getOrElse(Set.empty).take(1)),
          Some(cnv.`type`)
        )

      medicationCriteria =
        patRec
          .getTherapies
          .map(_.latest)
          .collectFirst {
            case th if th.medication.isDefined =>
              MedicationCriteria(
                Some(LogicalOperator.And),
                th.medication
                  .get
                  .map(_.asInstanceOf[Coding[ATC]])
                  .map(Tree(_)),
                Some(Set(Coding(MedicationUsage.Used)))
             )
          }

    } yield MTBQueryCriteria(
      Some(Set(icd10)),
      None,
      Some(Set(snvCriteria)),
      Some(Set(cnvCriteria)),
      None,
      None,
      medicationCriteria,
      None,
    )



  "SPI" must "have worked" in {
    serviceTry.isSuccess mustBe true
  }


  "Importing MTBPatientRecords" must "have worked" in {

    for {
      outcomes <-
        Future.traverse(dataSets)(service ! Save(_))
    } yield all (outcomes.map(_.isRight)) mustBe true 
    
  }

  // For implicit conversion of MTBFilters to predicate function
  import service.filterToPredicate


  val queryMode =
    Coding(Query.Mode.Local)


  "Query ResultSet" must "contain the total number of data sets for a query without criteria" in {

    for {
      result <-
        service ! Query.Submit(
          queryMode,
          None,
          None
//          MTBQueryCriteria(None,None,None,None,None,None,None,None)
        )

      query = result.value

      resultSet <-
        service.resultSet(query.id).map(_.value)

      summary = resultSet.summary(MTBFilters.empty)

      _ = summary.patientCount must equal (dataSets.size) 

      _ = summary.diagnostics.overallDistributions.tumorEntities.elements must not be empty

      _ = summary.medication.recommendations.distributionBySupportingVariant must not be empty

      _ = summary.medication.therapies.responseDistributionByTherapy must not be empty

    } yield succeed

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

      _ = all (queryCriteria.diagnoses.value.map(_.display)) must be (defined)  
      _ = all (queryCriteria.diagnoses.value.map(_.version)) must be (defined)  

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
