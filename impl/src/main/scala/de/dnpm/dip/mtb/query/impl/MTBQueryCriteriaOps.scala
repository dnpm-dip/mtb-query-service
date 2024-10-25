package de.dnpm.dip.mtb.query.impl


import scala.collection.mutable.Stack
import scala.util.chaining._
import cats.data.Ior
import cats.data.Ior.{
  Left,
  Right,
  Both
}
import de.dnpm.dip.coding.{
  Code,
  Coding
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.coding.hgvs.HGVS
import de.dnpm.dip.model.{
  Reference,
  Medications
}
import de.dnpm.dip.mtb.model.{
  MTBPatientRecord,
  MTBMedicationRecommendation,
  Fusion,
  SNV,
  CNV,
  Variant
}
import de.dnpm.dip.mtb.query.api._
import LogicalOperator.{And,Or}



private trait MTBQueryCriteriaOps
{

  private[impl] implicit class Extensions(criteria: MTBQueryCriteria){

    def getDiagnoses          = criteria.diagnoses.getOrElse(Set.empty)
    def getTumorMorphologies  = criteria.tumorMorphologies.getOrElse(Set.empty)
    def getSimpleVariants     = criteria.variants.flatMap(_.simpleVariants).getOrElse(Set.empty)
    def getCopyNumberVariants = criteria.variants.flatMap(_.copyNumberVariants).getOrElse(Set.empty)
    def getDnaFusions         = criteria.variants.flatMap(_.dnaFusions).getOrElse(Set.empty)
    def getRnaFusions         = criteria.variants.flatMap(_.rnaFusions).getOrElse(Set.empty)
    def getResponses          = criteria.responses.getOrElse(Set.empty)
    def getDrugs              = criteria.medication.map(_.drugs).getOrElse(Set.empty)

    def isEmpty: Boolean =
      (
        criteria.getDiagnoses          ++
        criteria.getTumorMorphologies  ++
        criteria.getSimpleVariants     ++
        criteria.getCopyNumberVariants ++
        criteria.getDnaFusions         ++
        criteria.getRnaFusions         ++
        criteria.getResponses          ++
        criteria.getDrugs
      )
      .isEmpty

    def nonEmpty = !criteria.isEmpty

    def intersect(other: MTBQueryCriteria): MTBQueryCriteria =
      MTBQueryCriteria(
        criteria.diagnoses.map(_ intersect other.getDiagnoses),
        criteria.tumorMorphologies.map(_ intersect other.getTumorMorphologies),
        criteria.variants.map(
          variants => VariantCriteria(
            variants.operator,
            variants.simpleVariants.map(_ intersect other.getSimpleVariants),
            variants.copyNumberVariants.map(_ intersect other.getCopyNumberVariants),
            variants.dnaFusions.map(_ intersect other.getDnaFusions),
            variants.rnaFusions.map(_ intersect other.getRnaFusions)
          )
        ),
        criteria.medication.map(
          med =>
            other.medication match {
              case Some(MedicationCriteria(_,drugs,_)) if drugs.nonEmpty => 
                med.copy(
                  drugs = med.drugs intersect drugs
                )
              case _ => 
                med
            }
        ),
        criteria.responses.map(_ intersect other.getResponses),
      )

    def &(other: MTBQueryCriteria) = criteria intersect other

  }


  private implicit class OperatorSyntax(op: LogicalOperator.Value){
 
    def apply(bs: Stack[Boolean]): Boolean =
      op match {
        case And => bs.reduceOption(_ && _).getOrElse(true)
        case Or  => bs.reduceOption(_ || _).getOrElse(true)
      }
  }
  

  import VariantCriteriaOps._


  private def snvMatchesOn(
    criteria: Set[SNVCriteria],
    snvs: Seq[SNV]
  )(
    implicit recommendations: Iterable[MTBMedicationRecommendation]
  ): Set[SNVCriteria] =
    criteria.filter(
      criterion =>
        snvs.exists(snv =>
          criterion.matches(snv) &&
          criterion.supporting.fold(true){
            case true  => snv.isSupporting
            case false => true 
          }
        )
    )


  private def cnvMatchesOn(
    criteria: Set[CNVCriteria],
    cnvs: Seq[CNV],
  )(
    implicit recommendations: Iterable[MTBMedicationRecommendation]
  ): Set[CNVCriteria] =
    criteria.filter(
      criterion =>
        cnvs.exists(cnv =>
          criterion.matches(cnv) &&
          criterion.supporting.fold(true){
            case true  => cnv.isSupporting
            case false => true 
          }
        )
    )

  private def fusionMatches[F <: Fusion[_ <: { def gene: Coding[HGNC] }]](
    criteria: Set[FusionCriteria],
    fusions: Seq[F]
  )(
    implicit recommendations: Iterable[MTBMedicationRecommendation]
  ): Set[FusionCriteria] = {

    import scala.language.reflectiveCalls

    criteria.filter(
      criterion => 
        fusions.exists(fusion =>
          criterion.matches(fusion) &&
          criterion.supporting.fold(true){
            case true  => fusion.isSupporting
            case false => true 
          }
        )
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

            implicit lazy val recommendations =
              record.getCarePlans
                .flatMap(_.medicationRecommendations.getOrElse(List.empty))


            val checks = new Stack[Boolean]
            val variantChecks = new Stack[Boolean]


            val diagnosisMatches =
              queryCriteria.diagnoses
                .collect {
                   case criteria if criteria.nonEmpty =>
                     (criteria intersect record.getDiagnoses.map(_.code).toSet)
                       .tap(matches => checks += matches.nonEmpty)
                }

            val morphologyMatches =
              queryCriteria.tumorMorphologies
                .collect {
                   case criteria if criteria.nonEmpty =>
                     (criteria intersect record.getHistologyReports.flatMap(_.results.tumorMorphology).map(_.value).toSet)
                       .tap(matches => checks += matches.nonEmpty)
                }


            val snvMatches =
              queryCriteria.variants
                .flatMap(_.simpleVariants)
                .collect {
                  case criteria if criteria.nonEmpty =>
                    snvMatchesOn(criteria,record.getNgsReports.flatMap(_.results.simpleVariants))
                      .tap(matches => variantChecks += matches.nonEmpty)
                }

            val cnvMatches =
              queryCriteria.variants
                .flatMap(_.copyNumberVariants)
                .collect {
                  case criteria if criteria.nonEmpty =>
                    cnvMatchesOn(criteria,record.getNgsReports.flatMap(_.results.copyNumberVariants))
                      .tap(
                        matches => variantChecks += matches.nonEmpty
                      )
                }

            val dnaFusionMatches =
              queryCriteria.variants
                .flatMap(_.dnaFusions)
                .collect {
                  case criteria if criteria.nonEmpty =>
                    fusionMatches(criteria,record.getNgsReports.flatMap(_.results.dnaFusions))
                      .tap(
                        matches => variantChecks += matches.nonEmpty
                      )
                }

            val rnaFusionMatches =
              queryCriteria.variants
                .flatMap(_.rnaFusions)
                .collect {
                  case criteria if criteria.nonEmpty =>
                    fusionMatches(criteria,record.getNgsReports.flatMap(_.results.rnaFusions))
                      .tap(
                        matches => variantChecks += matches.nonEmpty
                      )
                }

            val medicationMatches =
              queryCriteria.medication
                .collect {
                  case criteria if criteria.drugs.nonEmpty =>
                    matches(
                      criteria,
                      record.getCarePlans
                        .flatMap(_.medicationRecommendations.getOrElse(List.empty))
                        .map(_.medication),
                      record.getTherapies
                        .map(_.latest)
                        .flatMap(_.medication)
                    )
                    .tap(
                      matches => checks += matches.drugs.nonEmpty
                    )
                }

            val responseMatches =
              queryCriteria.responses
                .collect {
                   case criteria if criteria.nonEmpty =>
                     (criteria intersect record.getResponses.map(_.value).toSet)
                       .tap(matches => checks += matches.nonEmpty)
                }
           
                
            val variantOperator =
              queryCriteria.variants.flatMap(_.operator).getOrElse(operator)


            if (operator(checks :+ variantOperator(variantChecks)))
              Some(
                MTBQueryCriteria(
                  diagnosisMatches,
                  morphologyMatches,
                  queryCriteria.variants.map(_ =>
                    VariantCriteria(
                      Some(variantOperator),
                      snvMatches,
                      cnvMatches,
                      dnaFusionMatches,
                      rnaFusionMatches
                    )
                  ),
                  medicationMatches,
                  responseMatches,
                )
              )
            else 
              None
      }

}

private[impl] object MTBQueryCriteriaOps extends MTBQueryCriteriaOps

