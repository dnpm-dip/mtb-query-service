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
import de.dnpm.dip.service.query.QueryService.{
  Save,
  Saved
}
import de.dnpm.dip.connector.HttpConnector
import de.ekut.tbi.generators.Gen


class Tests extends AsyncFlatSpec
{

  import de.dnpm.dip.util.Completer.syntax._
  import de.dnpm.dip.mtb.gens.Generators._

  System.setProperty(Site.PROP,"UKx:Musterlingen")
  System.setProperty(HttpConnector.Type.property,"fake")
  System.setProperty(MTBLocalDB.dataGenProp,"0")


  implicit val rnd: Random = new Random

  implicit val querier: Querier = Querier("Dummy-Querier-ID")

 
  val serviceTry = MTBQueryService.getInstance

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

      ngs = patRec.getNgsReports.head

      snv = ngs.results.simpleVariants.get.head

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
      diagnoses = Some(Set(icd10)),
      geneAlterations = Some(
        GeneAlterations(
          Set(
            GeneAlterationCriteria(
              snv.gene,
              Some(GeneAlterationCriteria.OnSNV(None,snv.proteinChange))
            )
          )
        )
      ),
      medication = medicationCriteria,
    )



  "SPI" must "have worked" in {
    serviceTry.isSuccess mustBe true
  }


  "Importing MTBPatientRecords" must "have worked" in {

    for {
      outcomes <- Future.traverse(dataSets)(service ! Save(_))
    } yield all (outcomes) must matchPattern { case Right(Saved(_)) => } 
    
  }


  val queryMode = Coding(Query.Mode.Local)


  "Query ResultSet" must "contain the total number of data sets for a query without criteria" in {

    for {
      result <- service ! Query.Submit(queryMode,None,None)

      query = result.value

      resultSet <- service.resultSet(query.id).map(_.value)

    } yield resultSet.demographics(MTBFilters.empty).patientCount must equal (dataSets.size)  

  }


  it must "contain a non-empty list of correctly matching data sets for a query with criteria" in {

    import MTBQueryCriteriaOps._

    for {
      result <- service ! Query.Submit(queryMode,None,Some(genCriteria.next))

      query = result.value

      queryCriteria = query.criteria.value

      resultSet <- service.resultSet(query.id).map(_.value)

      patientMatches = resultSet.patientMatches(MTBFilters.empty)

      _ = patientMatches must not be empty

      _ = patientMatches.size must be < (dataSets.size) 

      matchingCriteria = patientMatches.map(_.matchingCriteria)

      _ = all (matchingCriteria) must be (defined)

    } yield forAll(matchingCriteria){ 
      matches => assert((queryCriteria intersect matches.value).nonEmpty)
    }

  }


  it must "contain the expected Patient for a query by supporting gene alteration" in { 
    
    // Get a MTBPatientRecord with supportingVariants
    val record =
      dataSets.find(
        _.getCarePlans.exists(
          _.medicationRecommendations.exists(
            _.exists(_.supportingVariants.isDefined)
          )
        )
      )
      .get

    val supportingAlteredGene =
      record.getCarePlans(1)
        .medicationRecommendations.get.head
        .supportingVariants.get.head
        .gene.get

    val queryCriteria =
      MTBQueryCriteria(
        geneAlterations = Some(
          GeneAlterations(
            Set(
              GeneAlterationCriteria(
                gene = supportingAlteredGene,
                supporting = Some(true),
                variant = None
              )
            )
          )
        )
      )

    for {

      result <- service ! Query.Submit(queryMode,None,Some(queryCriteria))

      query = result.value

      resultSet <- service.resultSet(query.id).map(_.value)

    } yield resultSet.patientRecord(record.id) must be (defined)

  }


  "Filtering" must "have worked" in {

    for {
      result <- service ! Query.Submit(queryMode,None,None)

      query = result.value

      resultSet <- service.resultSet(query.id).map(_.value)

      filter = MTBFilters.empty.copy(
        patient = PatientFilter.empty.copy(
          gender = Some(Set(Coding(Gender.Female)))
        )
      )

      patientMatches = resultSet.patientMatches(filter)

    } yield patientMatches.size must be < (dataSets.size)

  }


  "Relevance ranking" must "have worked on GeneAlterationInfo and TherapyResponses" in { 

    // Get a MTBPatientRecord with supportingVariants
    val record =
      dataSets.find(
        _.getCarePlans.exists(
          _.medicationRecommendations.exists(
            _.exists(_.supportingVariants.isDefined)
          )
        )
      )
      .get

    val queriedEntity = record.diagnoses.head.code

    val queriedAlteredGene =
      record.getCarePlans(1)
        .medicationRecommendations.get.head
        .supportingVariants.get.head
        .gene.get

    val queryCriteria =
      MTBQueryCriteria(
        diagnoses = Some(Set(queriedEntity)),
        geneAlterations = Some(
          GeneAlterations(
            Set(
              GeneAlterationCriteria(
                gene = queriedAlteredGene,
                supporting = Some(true)
              )
            )
          )
        )
      )

    for {

      result <- service ! Query.Submit(queryMode,None,Some(queryCriteria))

      query = result.value

      resultSet <- service.resultSet(query.id).map(_.value)

      geneAlterationsInfos = resultSet.geneAlterations()

      therapyResponses = resultSet.therapyResponses()

      // The first entrie(s) must match the queried attributes and have a non-zero ranking score
      _ = geneAlterationsInfos.takeWhile(r =>
        r.resource.entity == queriedEntity &&
        r.resource.gene == queriedAlteredGene &&
        r.resource.alteration.gene == queriedAlteredGene &&
        r.score > 0.0
      ) must not be (empty) 

      _ = therapyResponses.takeWhile(r =>
        r.resource.entity == queriedEntity &&
        r.resource.supportingAlteration.gene == queriedAlteredGene &&
        r.score > 0.0
      ) must not be (empty) 

      // Entries must be sorted in descending order of ranking score 
      _ = geneAlterationsInfos.map(_.score).reverse mustBe sorted

      _ = therapyResponses.map(_.score).reverse mustBe sorted

    } yield succeed // If this point is reached, test passed

  }


  "PreparedQuery" must "have been successfully created" in {

    for {
      result <- service ! PreparedQuery.Create("Dummy Prepared Query",genCriteria.next)
    } yield result.isRight mustBe true 

  }


  it must "have been successfully retrieved" in {

    for {
      result <- service ? PreparedQuery.Filter(Some(querier))

      _ = result must not be empty 

      query <- service ? result.head.id

    } yield query must be (defined)

  }



  "GeneAlteration matching" must "have worked" in { 

    import GeneAlterationExtensions._

    val snvs = dataSets.head.getNgsReports.head.results.simpleVariants.getOrElse(List.empty)
    val cnvs = dataSets.head.getNgsReports.head.results.copyNumberVariants.getOrElse(List.empty)

    forAll(snvs){ 
      snv =>

        val fullCriteria =
          GeneAlterationCriteria(
            gene = snv.gene,
            variant = Some(GeneAlterationCriteria.OnSNV(None,snv.proteinChange))
          )

        val geneOnlyCriteria =
          GeneAlterationCriteria(
            gene = snv.gene,
            variant = None
          )

        forAll(snv.geneAlterations.map(fullCriteria matches _))(_ mustBe true)
        forAll(snv.geneAlterations.map(geneOnlyCriteria matches _))(_ mustBe true)

        val (sameGeneCnvAlterations,differentGeneCnvAlterations) = cnvs.flatMap(_.geneAlterations).partition(_.gene.code == snv.gene.code)

        forAll(sameGeneCnvAlterations.map(fullCriteria matches _))(_ mustBe false)
        forAll(sameGeneCnvAlterations.map(geneOnlyCriteria matches _))(_ mustBe true)
        forAll(differentGeneCnvAlterations.map(geneOnlyCriteria matches _))(_ mustBe false)
    }

  }

}
