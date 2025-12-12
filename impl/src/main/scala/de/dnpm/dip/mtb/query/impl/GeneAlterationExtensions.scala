package de.dnpm.dip.mtb.query.impl


import de.dnpm.dip.util.Displays
import de.dnpm.dip.coding.Coding
import de.dnpm.dip.mtb.model.{
  CNV,
  SNV,
  DNAFusion,
  RNAFusion,
  RNASeq,
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



  private val cnvTypeMapping: Map[Coding[CNV.Type.Value],GeneAlteration.CNV.Type.Value] =
    CNV.Type.values
      .toList
      .collect { 
        case Loss          => Coding(Loss)          -> GeneAlteration.CNV.Type.Deletion
        case LowLevelGain  => Coding(LowLevelGain)  -> GeneAlteration.CNV.Type.Amplification
        case HighLevelGain => Coding(HighLevelGain) -> GeneAlteration.CNV.Type.Amplification
      }
      .toMap


  implicit class VariantGeneAlterationOps(val variant: Variant) extends AnyVal
  {

    def geneAlterations: Iterable[GeneAlteration] = 
      variant match {
        case snv: SNV =>
          Some(
            GeneAlteration.SNV(snv.gene,snv.proteinChange)
          )
          
        case cnv: CNV =>
          cnv.reportedAffectedGenes
            .getOrElse(Set.empty)
            .map(
              GeneAlteration.CNV(_,cnvTypeMapping(cnv.`type`))
            )

        case dna: DNAFusion =>
          List(
            GeneAlteration.Fusion(dna.fusionPartner5prime.gene,dna.fusionPartner3prime.gene),
            GeneAlteration.Fusion(dna.fusionPartner3prime.gene,dna.fusionPartner5prime.gene)
          )

        case rna: RNAFusion =>
          List(
            GeneAlteration.Fusion(rna.fusionPartner5prime.gene,rna.fusionPartner3prime.gene),
            GeneAlteration.Fusion(rna.fusionPartner3prime.gene,rna.fusionPartner5prime.gene)
          )

        case _: RNASeq => None
      }

  }


/*
  implicit class GeneAlterationCriteriaOps(val criteria: GeneAlterationCriteria) extends BooleanRelevanceMatcher[GeneAlteration]
  {

    import de.dnpm.dip.coding.hgvs.HGVS.extensions._

    override def check(alteration: GeneAlteration): Seq[Boolean] =

      alteration match {
        case snv: GeneAlteration.SNV =>
          Seq(criteria.gene.code == snv.gene.code) :++
            criteria.variant.collect {
              case crit: GeneAlterationCriteria.SNVCriteria if crit.proteinChange.isDefined =>
                crit.proteinChange.exists(g => snv.proteinChange.exists(_ matches g))
            }

        case cnv: GeneAlteration.CNV =>
          Seq(criteria.gene.code == cnv.gene.code) :++ 
            criteria.variant.collect {
              case crit: GeneAlterationCriteria.CNVCriteria if crit.copyNumberType.isDefined =>
                crit.copyNumberType.exists(_.collect(cnvTypeMapping) contains cnv.`type`)
            }
            
        case fusion: GeneAlteration.Fusion =>
          val fusionGenes = Set(fusion.gene.code,fusion.partner.code)

          Seq(fusionGenes(criteria.gene.code)) :++
            criteria.variant.collect {
              case crit: GeneAlterationCriteria.FusionCriteria if crit.partner.isDefined =>
                crit.partner.exists(gene => fusionGenes contains gene.code)
            }

        // Can't occur but needed for exhaustive pattern matching    
        case x: GeneAlteration.Unspecified => Seq(criteria.gene.code == x.gene.code)

      }

  }
*/


  implicit class GeneAlterationCriteriaOps(val criteria: GeneAlterationCriteria) extends BooleanRelevanceMatcher[GeneAlteration]
  {

    import de.dnpm.dip.coding.hgvs.HGVS.extensions._

    override def check(alteration: GeneAlteration): Seq[Boolean] =

      alteration match {
        case snv: GeneAlteration.SNV =>
          Seq(criteria.gene.code == snv.gene.code) :++
            criteria.variant.map {
              case crit: GeneAlterationCriteria.SNVCriteria => crit.proteinChange.map(g => snv.proteinChange.exists(_ matches g))

              case _ => Some(false) // Wrong alteration type
            }
            .getOrElse(Seq.empty[Boolean])

        case cnv: GeneAlteration.CNV =>
          Seq(criteria.gene.code == cnv.gene.code) :++ 
            criteria.variant.map {
              case crit: GeneAlterationCriteria.CNVCriteria => crit.copyNumberType.map(_.collect(cnvTypeMapping) contains cnv.`type`)

              case _ => Some(false) // Wrong alteration type
            }
            .getOrElse(Seq.empty[Boolean])
            
        case fusion: GeneAlteration.Fusion =>
          val fusionGenes = Set(fusion.gene.code,fusion.partner.code)

          Seq(fusionGenes(criteria.gene.code)) :++
            criteria.variant.map {
              case crit: GeneAlterationCriteria.FusionCriteria => crit.partner.map(gene => fusionGenes contains gene.code)
                
              case _ => Some(false) // Wrong alteration type
            }
            .getOrElse(Seq.empty[Boolean])

        // Can't occur but needed for exhaustive pattern matching    
        case x: GeneAlteration.Unspecified => Seq(criteria.gene.code == x.gene.code)

      }

  }


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
