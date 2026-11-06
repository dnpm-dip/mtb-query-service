package de.dnpm.dip.mtb.query.impl


import de.dnpm.dip.util.Displays
import de.dnpm.dip.model.Id
import de.dnpm.dip.mtb.model.{
  MTBMedicationRecommendation,
  Variant
}
import de.dnpm.dip.mtb.query.api.GeneAlteration


object GeneAlterationExtensions
{

  implicit val displays: Displays[GeneAlteration] =
    Displays[GeneAlteration]{ 

      case GeneAlteration.SNV(gene,proteinChange) =>
        s"${gene.display.getOrElse(gene.code)} ${proteinChange.map(_.value).getOrElse("SNV")}"

      case GeneAlteration.CNV(gene,typ) =>
        s"${gene.display.getOrElse(gene.code)} $typ"

      case GeneAlteration.Fusion(gene,partner) =>
        s"${Set(gene,partner).flatMap(_.display).mkString("-")} Fusion"

      case GeneAlteration.Unspecified(gene) =>
        s"${gene.display.getOrElse(gene.code)}"

    }


  type LinkedGeneAlteration = (Id[Variant],GeneAlteration)


  implicit def toAlteration(linkedAlteration: (Id[Variant],GeneAlteration)): GeneAlteration =
    linkedAlteration._2


  implicit class LinkedGeneAlterationOps(val linkedAlteration: (Id[Variant],GeneAlteration)) extends AnyVal 
  {

    /**
     * Check whether the alteration supports a therapy recommendation, i.e.
     * - if the variant it belongs to is referenced from a MTBMedicationRecommendation
     * - if the gene is specified on the GeneAlterationReference, ensure it matches the GeneAlteration
     * The latter is relevant esp. for CNVs, which can affect mutliple genes, (e.g. Genes A, B, ..., H),
     * but the supporting GeneAlterationReference might refenrece specifically Gene B as the one relevant for the recommendation.
     */
    def isSupporting(
      implicit recommendations: Seq[MTBMedicationRecommendation]
    ): Boolean = {

      val (variantId,alteration) = linkedAlteration

      recommendations.exists(
        _.supportingVariants.exists(
          _.exists(ref => 
            ref.variant.id == variantId &&
            ref.gene.fold(true)(_ == alteration.gene)
          )
        )
      )
    }

  }

}
