package de.dnpm.dip.mtb.query.impl


import de.dnpm.dip.util.Displays
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.model.Id
import de.dnpm.dip.mtb.model.{
  CNV,
  DNAFusion,
  Fusion,
  MTBMedicationRecommendation,
  RNAFusion,
  RNASeq,
  SNV,
  Variant
}
import CNV.Type._
import de.dnpm.dip.mtb.query.api.{
  GeneAlteration,
  GeneAlterations,
  GeneAlterationCriteria
}


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


  private val cnvTypeMapping: Map[Coding[CNV.Type.Value],GeneAlteration.CNV.Type.Value] =
    CNV.Type.values
      .toList
      .collect { 
        case Loss          => Coding(Loss)          -> GeneAlteration.CNV.Type.Deletion
        case LowLevelGain  => Coding(LowLevelGain)  -> GeneAlteration.CNV.Type.Amplification
        case HighLevelGain => Coding(HighLevelGain) -> GeneAlteration.CNV.Type.Amplification
      }
      .toMap


  implicit class VariantExtensions(val variant: Variant) extends AnyVal
  {

    def geneAlterations: Iterable[GeneAlteration] = 
      variant match {
        case snv: SNV =>
          Some(GeneAlteration.SNV(snv.gene,snv.proteinChange))
          
        case cnv: CNV =>
          cnv.reportedAffectedGenes
            .getOrElse(Set.empty)
            .map(GeneAlteration.CNV(_,cnvTypeMapping(cnv.`type`)))

        case dna: DNAFusion =>
          Some(
            GeneAlteration.Fusion(dna.fusionPartner5prime.gene,dna.fusionPartner3prime.gene),
          )

        case rna: RNAFusion =>
          Some(
            GeneAlteration.Fusion(rna.fusionPartner5prime.gene,rna.fusionPartner3prime.gene),
          )

        case _: RNASeq => None
      }


    def affectedGenes: Set[Coding[HGNC]] =
      variant match { 
        case snv: SNV => Set(snv.gene)

        case cnv: CNV => cnv.reportedAffectedGenes.getOrElse(Set.empty)

        case dna: DNAFusion => Set(dna.fusionPartner5prime.gene,dna.fusionPartner3prime.gene)

        case rna: RNAFusion => Set(rna.fusionPartner5prime.gene,rna.fusionPartner3prime.gene)

        case rnaSeq: RNASeq => rnaSeq.gene.toSet
      }


    def isSupporting(
      implicit recommendations: Seq[MTBMedicationRecommendation]
    ): Boolean =
      recommendations.exists(
        _.supportingVariants.exists(
          _.exists(_.variant.id == variant.id)
        )
      )

    def matches(criteria: GeneAlterationCriteria): Boolean = {

      import de.dnpm.dip.coding.hgvs.HGVS.extensions._

      (criteria.alteration,variant) match {

        case (None,anyVariant) => anyVariant.affectedGenes.contains(criteria.gene) 

        case (Some(crit: GeneAlterationCriteria.OnSNV),snv: SNV) =>
          (criteria.gene.code == snv.gene.code) && 
          crit.proteinChange.fold(true)(pattern => snv.proteinChange.exists(_ matches pattern)) &&
          crit.dnaChange.fold(true)(snv.dnaChange matches _)
        
        case (Some(crit: GeneAlterationCriteria.OnCNV),cnv: CNV) =>
          cnv.reportedAffectedGenes.exists(_.exists(_.code == criteria.gene.code)) &&
          crit.copyNumberType.fold(true)(_.exists(_.code == cnv.`type`.code))

        case (Some(crit: GeneAlterationCriteria.OnFusion),fusion: Fusion[_]) =>
          val fusionGenes = fusion.affectedGenes

          fusionGenes(criteria.gene) &&
          crit.partner.fold(true)(gene => fusionGenes contains gene)
          
        case _ => false // Wrong alteration type
      }

    }

  }


  implicit class GeneAlterationCriteriaMatcher(val criteria: GeneAlterationCriteria) extends BooleanRelevanceMatcher[GeneAlteration]
  {

    import de.dnpm.dip.coding.hgvs.HGVS.extensions._

    override def check(alteration: GeneAlteration): Seq[Boolean] =

      alteration match {
        case snv: GeneAlteration.SNV =>
          Seq(criteria.gene.code == snv.gene.code) :++
            criteria.alteration.map {
              case crit: GeneAlterationCriteria.OnSNV => crit.proteinChange.fold(true)(g => snv.proteinChange.exists(_ matches g))

              case _ => false // Wrong alteration type
            }

        case cnv: GeneAlteration.CNV =>
          Seq(criteria.gene.code == cnv.gene.code) :++ 
            criteria.alteration.map {
              case crit: GeneAlterationCriteria.OnCNV => crit.copyNumberType.fold(true)(_.collect(cnvTypeMapping) contains cnv.copyNumberType)

              case _ => false // Wrong alteration type
            }
            
        case fusion: GeneAlteration.Fusion =>
          val fusionGenes = Set(fusion.gene.code,fusion.partner.code)

          Seq(fusionGenes(criteria.gene.code)) :++
            criteria.alteration.map {
              case crit: GeneAlterationCriteria.OnFusion => crit.partner.fold(true)(gene => fusionGenes contains gene.code)
                
              case _ => false // Wrong alteration type
            }

        // Can't occur but needed for exhaustive pattern matching    
        case x: GeneAlteration.Unspecified => Seq(criteria.gene.code == x.gene.code)

      }

  }

  //TODO: Remove by refactoring raking of "diagnosticDistributionsByAlteration"
  implicit class GeneAlterationsOps(val queriedAlterations: Option[GeneAlterations]) extends AnyVal
  {
    def score(alteration: GeneAlteration): Double =
      queriedAlterations
        .flatMap(
          _.items
           .map(_ score alteration)
           .maxOption
        )
        .getOrElse(0.0)

  }

}
