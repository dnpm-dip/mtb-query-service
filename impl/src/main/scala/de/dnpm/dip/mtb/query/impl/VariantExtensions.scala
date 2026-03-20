package de.dnpm.dip.mtb.query.impl


import de.dnpm.dip.coding.Coding
import de.dnpm.dip.coding.hgnc.HGNC
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
  GeneAlterationCriteria
}


object VariantExtensions
{

  private val cnvTypeMapping: Map[Coding[CNV.Type.Value],GeneAlteration.CNV.Type.Value] =
    CNV.Type.values
      .toList
      .collect { 
        case Loss          => Coding(Loss)          -> GeneAlteration.CNV.Type.Deletion
        case LowLevelGain  => Coding(LowLevelGain)  -> GeneAlteration.CNV.Type.Amplification
        case HighLevelGain => Coding(HighLevelGain) -> GeneAlteration.CNV.Type.Amplification
      }
      .toMap


  implicit class VariantOps(val variant: Variant) extends AnyVal
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

      // If the gene is specified to be wild-type, the variant mustn't affect it
      if (criteria.wildtype contains true) !(variant.affectedGenes contains criteria.gene)

      // Else check whether the alteration criteria match the variant, if specified
      else (criteria.alteration,variant) match {

        // If no alteration type is specified, just check if the variant affects the queried gene 
        case (None,_) => variant.affectedGenes contains criteria.gene

        case (Some(crit: GeneAlterationCriteria.OnSNV), snv: SNV) =>
          (criteria.gene.code == snv.gene.code) && 
          crit.proteinChange.fold(true)(pattern => snv.proteinChange.exists(_ matches pattern)) &&
          crit.dnaChange.fold(true)(snv.dnaChange matches _)
        
        case (Some(crit: GeneAlterationCriteria.OnCNV), cnv: CNV) =>
          cnv.reportedAffectedGenes.exists(_.exists(_.code == criteria.gene.code)) &&
          crit.copyNumberType.fold(true)(_.exists(_.code == cnv.`type`.code))

        case (Some(crit: GeneAlterationCriteria.OnFusion), fusion: Fusion[_]) =>
          val fusionGenes = fusion.affectedGenes
          fusionGenes(criteria.gene) &&
          crit.partner.fold(true)(gene => fusionGenes contains gene)
          
        case _ => false // Wrong alteration type
      }

    }

  }
}
