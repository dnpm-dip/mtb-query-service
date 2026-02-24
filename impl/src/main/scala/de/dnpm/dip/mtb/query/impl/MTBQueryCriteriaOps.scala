package de.dnpm.dip.mtb.query.impl


import scala.collection.mutable.Stack
import scala.util.chaining._
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.model.Medications
import de.dnpm.dip.mtb.model.{
  MTBPatientRecord,
  MTBMedicationRecommendation,
  SomaticNGSReport,
}
import de.dnpm.dip.mtb.query.api._
import LogicalOperator.{And,Or}



private trait MTBQueryCriteriaOps
{

  private[impl] implicit class Extensions(criteria: MTBQueryCriteria){

    def getDiagnoses          = criteria.diagnoses.getOrElse(Set.empty)
    def getTumorMorphologies  = criteria.tumorMorphologies.getOrElse(Set.empty)
    def getResponses          = criteria.responses.getOrElse(Set.empty)
    def getDrugs              = criteria.medication.map(_.items).getOrElse(Set.empty)

    def isEmpty: Boolean =
      (
        criteria.getDiagnoses          ++
        criteria.getTumorMorphologies  ++
        criteria.geneAlterations.map(_.items).getOrElse(Set.empty)  ++
        criteria.getResponses          ++
        criteria.getDrugs
      )
      .isEmpty

    def nonEmpty = !criteria.isEmpty

    def intersect(other: MTBQueryCriteria): MTBQueryCriteria =
      MTBQueryCriteria(
        criteria.diagnoses.map(_ intersect other.getDiagnoses),
        criteria.tumorMorphologies.map(_ intersect other.getTumorMorphologies),
        criteria.geneAlterations.map(
          alterations => GeneAlterations(
            alterations.operator,
            alterations.items intersect other.geneAlterations.map(_.items).getOrElse(Set.empty)
          )
        ),
        criteria.medication.map(
          med =>
            other.medication match {
              case Some(MedicationCriteria(_,items,_)) if items.nonEmpty => 
                med.copy(
                  items = med.items intersect items
                )
              case _ => 
                med
            }
        ),
        criteria.responses.map(_ intersect other.getResponses),
      )

    def &(other: MTBQueryCriteria) = criteria intersect other

  }


  private implicit def toBiFunction(op: LogicalOperator.Value): (Boolean,Boolean) => Boolean =
    op match {
      case And => _ & _
      case Or  => _ | _
    }


  private def matches(
    geneAlterations: GeneAlterations,
    ngsReports: List[SomaticNGSReport]
  )(
    implicit recommendations: Seq[MTBMedicationRecommendation]
  ): GeneAlterations = {

    import GeneAlterationExtensions._

    val alterationsByGene =
      ngsReports
        .flatMap(_.variants.flatMap(_.geneAlterations))
        .groupBy(_.gene.code)


    val fulfilled: GeneAlterationCriteria => Boolean = {
      criterion =>
        criterion.wildtype match {

          // If the wildtype gene is queried for, then it shouldn't occur among the altered genes
          case Some(true) => !alterationsByGene.contains(criterion.gene.code)

          // Else check if a matching alteration exists for the gene...
          case _ =>
            alterationsByGene.get(criterion.gene.code)
              .flatMap(_.find(criterion matches _))
              .map { 
                // ... and if the alteration is specifically supposed to be supporting a therapy recommendation, check this in addition
                alteration => criterion.supporting match {
                  case Some(true) =>
                    recommendations.exists(
                      _.supportingVariants.exists(
                        _.exists(_.variant.id == alteration.variant)
                      )
                    )

                  // If unspecified, no need to check for occurrence as supportingVariant 
                  case _ => true
                }
              }
              .getOrElse(false)

        }
    }


    val fulfilledCriteria =
      geneAlterations.items
        .map(crit => Option.when(fulfilled(crit))(crit))

    val operator =
      geneAlterations.operator.getOrElse(Or)


    GeneAlterations(
      Some(operator),
      if (fulfilledCriteria.map(_.isDefined).reduceOption(operator).getOrElse(true))
        fulfilledCriteria.flatten
      else
        Set.empty
    )

  }


  private def matches(
    criteria: MedicationCriteria,
    recommendedDrugs: List[Set[Coding[Medications]]],
    usedDrugs: List[Set[Coding[Medications]]],
  ): MedicationCriteria = {

    import MedicationUsage._
    import de.dnpm.dip.util.Tree

    def drugMatches(
      queriedDrugs: Set[Tree[Coding[Medications]]],
      drugSets: List[Set[String]],
      op: LogicalOperator.Value
    ): Set[Tree[Coding[Medications]]] = 
      op match {
        case Or =>
          // Pick those elements among the sub-trees which occur in any of the drug name sets
          queriedDrugs.flatMap( 
            _.find(coding => drugSets.exists(_ exists (name => coding.display.exists(name contains _.toLowerCase))))
             .map(Tree(_))
          )

        case And =>
          drugSets
            .view
            .map(
              drugSet =>
                queriedDrugs.map(
                  _.find(coding => drugSet.exists(name => coding.display.exists(name contains _.toLowerCase)))
                )
            )
            .collectFirst {
              case ts if ts.forall(_.isDefined) => ts.flatten.map(Tree(_))
            }
            .getOrElse(Set.empty)
      }


    val queriedDrugs =
      criteria.expandedDrugs

    val operator =
      criteria.operator.getOrElse(Or)

    lazy val recommendedDrugNames =
      recommendedDrugs.map(_.flatMap(_.display.map(_.toLowerCase)))

    lazy val usedDrugNames =
      usedDrugs.map(_.flatMap(_.display.map(_.toLowerCase)))

    val matches =
      criteria.usage
        .getOrElse(Set.empty)
        .collect { case MedicationUsage(value) => value } match {
      
          case s if s.contains(Recommended) && s.contains(Used) =>
            drugMatches(queriedDrugs,recommendedDrugNames,operator) & drugMatches(queriedDrugs,usedDrugNames,operator)
      
          case s if s.contains(Recommended) =>
            drugMatches(queriedDrugs,recommendedDrugNames,operator)
      
          case s if s.contains(Used) =>
            drugMatches(queriedDrugs,usedDrugNames,operator)
      
          case _ =>
            drugMatches(queriedDrugs,recommendedDrugNames,operator) | drugMatches(queriedDrugs,usedDrugNames,operator)
      
        }

     MedicationCriteria(
       criteria.operator,
       matches.map(_.element),
       criteria.usage
     )

  }


  def criteriaMatcher(
    operator: LogicalOperator.Value = And
  ): MTBQueryCriteria => (MTBPatientRecord => Option[MTBQueryCriteria]) =

     _ match {

        // If criteria object is empty, i.e. no query criteria are defined at all, any patient record matches
        case criteria if criteria.isEmpty => 
          record => Some(criteria)
 
 
        case queryCriteria => 

          record =>

            val checks = new Stack[Boolean]

            val diagnosisMatches =
              queryCriteria.diagnoses
                .collect {
                   case criteria if criteria.nonEmpty =>
                     (criteria intersect record.diagnoses.map(_.code).toList.toSet)
                       .tap(matches => checks += matches.nonEmpty)
                }

            val morphologyMatches =
              queryCriteria.tumorMorphologies
                .collect {
                   case criteria if criteria.nonEmpty =>
                     (criteria intersect record.getHistologyReports.map(_.results.tumorMorphology.value).toSet)
                       .tap(matches => checks += matches.nonEmpty)
                }

            implicit val recommendations: Seq[MTBMedicationRecommendation] =
              record.getCarePlans.flatMap(_.medicationRecommendations.getOrElse(List.empty))

            val geneAlterationMatches =
              queryCriteria.geneAlterations.collect {
                case alterations if alterations.items.nonEmpty =>
                  matches(
                    alterations,
                    record.getNgsReports,
                  )
                  .tap(
                    matches => checks += matches.items.nonEmpty
                  )
              }

            val medicationMatches =
              queryCriteria.medication
                .collect {
                  case criteria if criteria.items.nonEmpty =>
                    matches(
                      criteria,
                      record.getCarePlans
                        .flatMap(_.medicationRecommendations.getOrElse(List.empty))
                        .map(_.medication),
                      record.getSystemicTherapies
                        .map(_.latest)
                        .flatMap(_.medication)
                    )
                    .tap(
                      matches => checks += matches.items.nonEmpty
                    )
                }

            val responseMatches =
              queryCriteria.responses
                .collect {
                   case criteria if criteria.nonEmpty =>
                     (criteria intersect record.getResponses.map(_.value).toSet)
                       .tap(matches => checks += matches.nonEmpty)
                }
           
                
            if (checks.reduceOption(operator).getOrElse(true))
              Some(
                MTBQueryCriteria(
                  diagnosisMatches,
                  morphologyMatches,
                  geneAlterationMatches,
                  medicationMatches,
                  responseMatches,
                )
              )
            else 
              None
      }

}

private[impl] object MTBQueryCriteriaOps extends MTBQueryCriteriaOps

