package de.dnpm.dip.mtb.query.impl


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
import de.dnpm.dip.coding.hgvs.HGVS
import de.dnpm.dip.model.Reference
import de.dnpm.dip.mtb.model.{
  MTBPatientRecord,
  MTBMedicationRecommendation,
  SNV,
  CNV,
  Variant
}
import de.dnpm.dip.mtb.query.api._



private trait MTBQueryCriteriaOps
{

  private[impl] implicit class Extensions(criteria: MTBQueryCriteria){

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
        criteria.simpleVariants.map(_ intersect other.getSimpleVariants),
        criteria.copyNumberVariants.map(_ intersect other.getCopyNumberVariants),
        criteria.dnaFusions.map(_ intersect other.getDnaFusions),
        criteria.rnaFusions.map(_ intersect other.getRnaFusions),
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


  private def checkMatches(
    bs: Boolean*
  )(
    strict: Boolean
  ): Boolean =
    if (strict)
      bs forall (_ == true)
    else
      bs exists (_ == true)


  private def matches[T](
    criteria: Option[Set[T]],
    values: => Set[T]
  ): (Option[Set[T]],Boolean) =
    criteria match {
      case Some(set) if set.nonEmpty =>
        (set intersect values)
          .pipe {
            case matches if matches.nonEmpty =>
              Some(matches) -> true

            case _ =>
              None -> false
          }

      case _ => None -> true
    }


  private def matches[T](
    criterion: Option[T],
    value: Option[T]
  ): Boolean =
    criterion
      .map(c => value.exists(_ == c))
      .getOrElse(true)



  import scala.language.implicitConversions

  implicit def optBooleantoBoolean(opt: Option[Boolean]): Boolean =
    opt.getOrElse(false)


  private def snvsMatch(
    criteria: Option[Set[SNVCriteria]],
    snvs: => Seq[SNV]
  )(
    implicit supportingVariants: Seq[Reference[Variant]]
  ): (Option[Set[SNVCriteria]],Boolean) = {

    import HGVS.extensions._

    criteria match {
      case Some(set) if set.nonEmpty =>
        set.filter {
          case SNVCriteria(gene,dnaChange,proteinChange,supporting) =>
            snvs.find(
              snv =>
                checkMatches(
                  matches(gene,snv.gene),
                  dnaChange.map(g => snv.dnaChange.exists(_ matches g)).getOrElse(true),
                  proteinChange.map(g => snv.proteinChange.exists(_ matches g)).getOrElse(true)
                )(
                  true
                ) 
            )
            .exists { 
              case snv if supporting => supportingVariants.exists(_.id.exists(_ == snv.id))
              case _ => true 
            }
            
        }
        .pipe {
          case matches if matches.nonEmpty =>  
            Some(matches) -> true

          case _ =>
            None -> false
        }

      case _ => None -> true
    }

  }

  private def cnvsMatch(
    criteria: Option[Set[CNVCriteria]],
    cnvs: => Seq[CNV]
  )(
    implicit supportingVariants: Seq[Reference[Variant]]
  ): (Option[Set[CNVCriteria]],Boolean) =
    criteria match {
      case Some(set) if set.nonEmpty =>
        set.filter {
          case CNVCriteria(affectedGenes,typ,supporting) =>
            cnvs.find(
              cnv =>
                checkMatches(
                  affectedGenes match {
                    case Some(genes) if genes.nonEmpty => cnv.reportedAffectedGenes.exists(_.intersect(genes).nonEmpty)
                    case _ => true
                  },
                  typ.map(_ == cnv.`type`).getOrElse(true)
                )(
                  true
                ) 
              )
              .exists { 
                case snv if supporting => supportingVariants.exists(_.id.exists(_ == snv.id))
                case _ => true 
              }
        }
        .pipe {
          case matches if matches.nonEmpty =>  
            Some(matches) -> true

          case _ =>
            None -> false
        }

      case _ => None -> true
    }


  private def medicationsMatch(
    criteria: Option[MedicationCriteria],
    recommendedDrugs: => List[Set[Coding[ATC]]],
    usedDrugs: => List[Set[Coding[ATC]]],
  ): (Option[MedicationCriteria],Boolean) = {

    import MedicationUsage._
    import LogicalOperator.{And,Or}

    import de.dnpm.dip.util.Tree

/*
    def combinedMatches(
      queriedDrugs: Set[Tree[Coding[ATC]]],
      drugSets: List[Set[String]],
    ): Set[Coding[ATC]] =
      if (queriedDrugs.nonEmpty){

        val q = queriedDrugs.head

        // Iterate over drugSets to accumulate both
        // the matching entries from the current Coding tree
        // and those drugSets which do lead to a match
        val (drugMatches,matchingDrugSets) =
          drugSets.foldLeft(
            Set.empty[Coding[ATC]] -> List.empty[Set[String]]
          ){
            case ((drugMatches,acc),drugSet) =>
              val matches =
                drugSet.foldLeft(
                  Seq.empty[Coding[ATC]]
                )(
                  (acc2,drug) => acc2 ++ q.filter(_.display.exists(drug contains _.toLowerCase))  
                )
                
              if (matches.nonEmpty) 
                (drugMatches ++ matches, acc :+ drugSet) 
              else 
                (drugMatches, acc)
          }

        // If there are matches, proceed with the remaining queried drugs,
        // using those drugSet which had a match
        if (drugMatches.nonEmpty)
          drugMatches.toSet ++ combinedMatches(queriedDrugs.tail,matchingDrugSets)
        // else break here
        else 
          drugMatches.toSet
          
      } else {
        Set.empty  
      }
*/

    def drugMatches(
      queriedDrugs: Set[Tree[Coding[ATC]]],
      drugSets: List[Set[String]],
      op: LogicalOperator.Value
    ): Set[Tree[Coding[ATC]]] = {

      op match {
        case Or =>
          // Pick those elements among the sub-trees which occur in any of the drug name sets
          queriedDrugs.flatMap( 
            _.find(coding => drugSets.exists(_ exists (name => coding.display.exists(name contains _.toLowerCase))))
             .map(Tree(_))
          )
          
        case And =>
          drugSets.view.map(
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
    }


    criteria match {
      case Some(MedicationCriteria(op,queriedDrugs,usage)) if queriedDrugs.nonEmpty => 

        lazy val recommendedDrugNames =
          recommendedDrugs.map(_.flatMap(_.display.map(_.toLowerCase)))

        lazy val usedDrugNames =
          usedDrugs.map(_.flatMap(_.display.map(_.toLowerCase)))

        val operator =
          op.getOrElse(Or)

        usage
          .getOrElse(Set.empty)
          .collect { case MedicationUsage(value) => value }
          .pipe {

            case s if s.contains(Recommended) && s.contains(Used) =>
              drugMatches(queriedDrugs,recommendedDrugNames,operator) & drugMatches(queriedDrugs,usedDrugNames,operator)

            case s if s.contains(Recommended) =>
              drugMatches(queriedDrugs,recommendedDrugNames,operator)

            case s if s.contains(Used) =>
              drugMatches(queriedDrugs,usedDrugNames,operator)

            case _ =>
              drugMatches(queriedDrugs,recommendedDrugNames,operator) | drugMatches(queriedDrugs,usedDrugNames,operator)

          }
          .pipe {
            case matches if matches.nonEmpty =>
              Some(MedicationCriteria(op,matches,usage)) -> true
            case _ =>
              None  -> false
          }

      case _ => None -> true
    }

  }

/*
  private def medicationsMatch(
    criteria: Option[MedicationCriteria],
    recommendedDrugs: => List[Set[Coding[ATC]]],
    usedDrugs: => List[Set[Coding[ATC]]],
  ): (Option[MedicationCriteria],Boolean) = {

    import MedicationUsage._
    import LogicalOperator.{And,Or}

    def drugMatches(
      queriedDrugs: Set[Coding[ATC]],
      drugNames: List[Set[String]],
      op: LogicalOperator
    ): Set[Coding[ATC]] =
      op match {
        case Or =>
          // for each queried drug, check if any drug name set exists containing the queried drug's name
          queriedDrugs.filter( 
            cdng => drugNames exists (_ exists (name => cdng.display.exists(name contains _.toLowerCase)))
          )
        case And =>
          // Check if a drug name set (i.e. combination therapy) exists of which all names occur in the queried drugs
          if (drugNames exists (_ forall (name => queriedDrugs exists (_.display.exists(name contains _.toLowerCase)))))
            queriedDrugs
          else 
            Set.empty
      }

    criteria match {
      case Some(MedicationCriteria(op,queriedDrugs,usage)) if queriedDrugs.nonEmpty => 

        lazy val recommendedDrugNames =
          recommendedDrugs.map(_.flatMap(_.display.map(_.toLowerCase)))

        lazy val usedDrugNames =
          usedDrugs.map(_.flatMap(_.display.map(_.toLowerCase)))

        val operator =
          op.getOrElse(Or)

        usage
          .getOrElse(Set.empty)
          .collect { case MedicationUsage(value) => value }
          .pipe {

            case s if s.contains(Recommended) && s.contains(Used) =>
              drugMatches(queriedDrugs,recommendedDrugNames,operator) & drugMatches(queriedDrugs,usedDrugNames,operator)

            case s if s.contains(Recommended) =>
              drugMatches(queriedDrugs,recommendedDrugNames,operator)

            case s if s.contains(Used) =>
              drugMatches(queriedDrugs,usedDrugNames,operator)

            case _ =>
              drugMatches(queriedDrugs,recommendedDrugNames,operator) | drugMatches(queriedDrugs,usedDrugNames,operator)

          }
          .pipe {
            case matches if matches.nonEmpty =>
              Some(MedicationCriteria(op,matches,usage)) -> true
            case _ =>
              None  -> false
          }

      case _ => None -> true
    }

  }
*/

  def criteriaMatcher(
    strict: Boolean = true
  ): MTBQueryCriteria => (MTBPatientRecord => Option[MTBQueryCriteria]) = {

     _ match {

        // If criteria object is empty, i.e. no query criteria are defined at all, any patient record matches
        case criteria if criteria.isEmpty => 
          record => Some(criteria)

          
        case criteria => 

          record =>

            implicit lazy val supportingVariants =
              record.getCarePlans
                .flatMap(_.medicationRecommendations.getOrElse(List.empty))
                .flatMap(_.supportingVariants.getOrElse(List.empty)) 

            val (diagnosisMatches, diagnosesFulfilled) =
              matches(
                criteria.diagnoses,
                record.getDiagnoses
                  .map(_.code)
                  .toSet
              )

            val (morphologyMatches, morphologyFulfilled) =
              matches(
                criteria.tumorMorphologies,
                record.getHistologyReports
                  .flatMap(_.results.tumorMorphology)
                  .map(_.value)
                  .toSet
              )

            val (snvMatches, snvsFulfilled) =
              snvsMatch(
                criteria.simpleVariants,
                record
                  .getNgsReports
                  .flatMap(_.results.simpleVariants)
              )

            val (cnvMatches, cnvsFulfilled) =
              cnvsMatch(
                criteria.copyNumberVariants,
                record.getNgsReports
                  .flatMap(_.results.copyNumberVariants)
              )

            val (medicationMatches, medicationFulfilled) =
              medicationsMatch(
                criteria.medication,
                record.getCarePlans
                  .flatMap(_.medicationRecommendations.getOrElse(List.empty))
                  .map(_.medication),
                record.getTherapies
                  .map(_.latest)
                  .flatMap(_.medication)
              )


            val (responseMatches, responseFulfilled) =
              matches(
                criteria.responses,
                record.getResponses
                  .map(_.value)
                  .toSet
              )

          if (
            checkMatches(
              diagnosesFulfilled,
              morphologyFulfilled,
              snvsFulfilled,
              cnvsFulfilled,
              //TODO: DNA-/RNA-Fusions
              medicationFulfilled,
              responseFulfilled
            )(
              strict
            )
          )
            Some(
              MTBQueryCriteria(
                diagnosisMatches,
                morphologyMatches,
                snvMatches,
                cnvMatches,
                None, //TODO: DNA-Fusions
                None, //TODO: RNA-Fusions
                medicationMatches,
                responseMatches,
              )
            )
          else 
            None
      }
  }

}

private object MTBQueryCriteriaOps extends MTBQueryCriteriaOps

