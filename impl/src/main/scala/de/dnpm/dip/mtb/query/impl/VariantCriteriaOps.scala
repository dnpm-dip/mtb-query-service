package de.dnpm.dip.mtb.query.impl


import scala.util.chaining._
import de.dnpm.dip.model.Reference
import de.dnpm.dip.mtb.model.{
  Variant,
  SNV,
  CNV,
  Fusion,
  DNAFusion,
  RNAFusion
}
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.coding.hgvs.HGVS
import de.dnpm.dip.mtb.query.api._
import LogicalOperator.{
  And,
  Or
}


object VariantCriteriaOps
{

  private type CanHaveSupportingVariants = {
    def supportingVariants: Option[List[Reference[Variant]]]
  }

  import scala.language.reflectiveCalls


  implicit class VariantOps(val variant: Variant) extends AnyVal
  {

    def isSupporting[T <: CanHaveSupportingVariants](
      implicit ts: Iterable[T]
    ): Boolean =
      ts.exists(
        _.supportingVariants.exists(
          _.exists(_.id.exists(_ == variant.id))
        )
      )

  }


  private def checkMatches(
    bs: Boolean*
  )(
    op: LogicalOperator.Value
  ): Boolean =
    op match {
      case And => bs forall (_ == true)
      case Or  => bs exists (_ == true)
    }


  implicit class SNVCriteriaOps(val criteria: SNVCriteria) extends AnyVal
  {

    import HGVS.extensions._

    def matches(snv: SNV): Boolean =
      checkMatches(
        criteria.gene.map(g => snv.gene.exists(_.code == g.code)).getOrElse(true),
        criteria.dnaChange.map(g => snv.dnaChange.exists(_ matches g)).getOrElse(true),
        criteria.proteinChange.map(g => snv.proteinChange.exists(_ matches g)).getOrElse(true)
      )(
        And
      )

  }
  

  implicit class CNVCriteriaOps(val criteria: CNVCriteria) extends AnyVal
  {

    def matches(cnv: CNV): Boolean =
      checkMatches(
        criteria.affectedGenes match {
          case Some(queriedGenes) if queriedGenes.nonEmpty =>
            cnv.reportedAffectedGenes.exists(cnvGenes => queriedGenes.forall(g => cnvGenes.exists(_.code == g.code)))

          case _ => true
        },
        criteria.`type`.map(_ == cnv.`type`).getOrElse(true)
      )(
        And
      )

  }
  

  implicit class FusionCriteriaOps[F <: Fusion[_ <: { def gene: Coding[HGNC] }]](
    val criteria: FusionCriteria
  )
  extends AnyVal
  {

    def matches(fusion: F): Boolean =
      checkMatches(
        criteria.fusionPartner5pr.map(_.code == fusion.fusionPartner5prime.gene.code).getOrElse(true),
        criteria.fusionPartner3pr.map(_.code == fusion.fusionPartner3prime.gene.code).getOrElse(true)
      )(
        And
      )

  }


/*  
  implicit class SNVCriteriaOps(val criteria: SNVCriteria) extends AnyVal
  {

    import HGVS.extensions._

    def matches[T <: CanHaveSupportingVariants](
      snv: SNV
    )(
      implicit ts: Iterable[T]
    ): Boolean =
      checkMatches(
        criteria.gene.map(g => snv.gene.exists(_.code == g.code)).getOrElse(true),
        criteria.dnaChange.map(g => snv.dnaChange.exists(_ matches g)).getOrElse(true),
        criteria.proteinChange.map(g => snv.proteinChange.exists(_ matches g)).getOrElse(true),
        criteria.supporting.map {
          case true  => snv.isSupporting
          case false => true 
        }
        .getOrElse(true)
      )(
        And
      )

  }
  

  implicit class CNVCriteriaOps(val criteria: CNVCriteria) extends AnyVal
  {

    def matches[T <: CanHaveSupportingVariants](
      cnv: CNV
    )(
      implicit ts: Iterable[T]
    ): Boolean =
      checkMatches(
        criteria.affectedGenes match {
          case Some(queriedGenes) if queriedGenes.nonEmpty =>
            cnv.reportedAffectedGenes.exists(cnvGenes => queriedGenes.forall(g => cnvGenes.exists(_.code == g.code)))

          case _ => true
        },
        criteria.`type`.map(_ == cnv.`type`).getOrElse(true),
        criteria.supporting.map {
          case true  => cnv.isSupporting
          case false => true 
        }
        .getOrElse(true)
      )(
        And
      )

  }
  

  implicit class FusionCriteriaOps[F <: Fusion[_ <: { def gene: Coding[HGNC] }]](
    val criteria: FusionCriteria
  )
  extends AnyVal
  {

    def matches[T <: CanHaveSupportingVariants](
      fusion: F
    )(
      implicit ts: Iterable[T]
    ): Boolean =
      checkMatches(
        criteria.fusionPartner5pr.map(_.code == fusion.fusionPartner5prime.gene.code).getOrElse(true),
        criteria.fusionPartner3pr.map(_.code == fusion.fusionPartner3prime.gene.code).getOrElse(true),
        criteria.supporting.map {
          case true  => fusion.isSupporting
          case false => true 
        }
        .getOrElse(true)
      )(
        And
      )

  }
*/ 


}
