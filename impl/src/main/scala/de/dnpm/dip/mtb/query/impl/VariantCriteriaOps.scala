package de.dnpm.dip.mtb.query.impl


import de.dnpm.dip.model.GeneAlterationReference
import de.dnpm.dip.mtb.model.{
  Variant,
  SNV,
  CNV,
  Fusion,
}
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.coding.hgvs.HGVS
import de.dnpm.dip.mtb.query.api._


object VariantCriteriaOps
{

  private type CanHaveSupportingVariants = {
    def supportingVariants: Option[List[GeneAlterationReference[Variant]]]
  }

  import scala.language.reflectiveCalls


  implicit class VariantOps(val variant: Variant) extends AnyVal
  {

    def isSupporting[T <: CanHaveSupportingVariants](
      implicit ts: Iterable[T]
    ): Boolean =
      ts.exists(
        _.supportingVariants.exists(
          _.exists(_.variant.id == variant.id)
        )
      )

  }



  sealed trait RelevanceMatcher[-T] extends Any {

    def check(t: T): Seq[Boolean]

    def score(t: T): Double = { 
      val checks = check(t)
      checks.count(_ == true).toDouble/checks.size
    }

    def matches(t: T): Boolean =
      check(t).forall(_ == true)
  }



  implicit class SNVCriteriaOps(val criteria: SNVCriteria) extends AnyVal with RelevanceMatcher[SNV]
  {
    import HGVS.extensions._

    override def check(snv: SNV): Seq[Boolean] =
      Seq(
        criteria.gene.map(g => snv.gene.code == g.code).getOrElse(true),
        criteria.dnaChange.map(g => snv.dnaChange matches g).getOrElse(true),
        criteria.proteinChange.map(g => snv.proteinChange.exists(_ matches g)).getOrElse(true)
      )
  }
  

  implicit class CNVCriteriaOps(val criteria: CNVCriteria) extends AnyVal with RelevanceMatcher[CNV]
  {
    override def check(cnv: CNV): Seq[Boolean] =
      Seq(
        criteria.affectedGenes match {
          case Some(queriedGenes) if queriedGenes.nonEmpty =>
            cnv.reportedAffectedGenes.exists(cnvGenes => queriedGenes.forall(g => cnvGenes.exists(_.code == g.code)))

          case _ => true
        },
        criteria.`type`.map(_ == cnv.`type`).getOrElse(true)
      )
  }
  

  implicit class FusionCriteriaOps[F <: Fusion[_ <: { def gene: Coding[HGNC] }]](
    val criteria: FusionCriteria
  )
  extends AnyVal with RelevanceMatcher[F]
  {
    override def check(fusion: F): Seq[Boolean] =
      Seq(
        criteria.fusionPartner5pr.map(_.code == fusion.fusionPartner5prime.gene.code).getOrElse(true),
        criteria.fusionPartner3pr.map(_.code == fusion.fusionPartner3prime.gene.code).getOrElse(true)
      )
  }


}
