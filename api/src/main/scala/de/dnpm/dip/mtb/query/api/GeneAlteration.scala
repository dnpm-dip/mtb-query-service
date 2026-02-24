package de.dnpm.dip.mtb.query.api


import de.dnpm.dip.coding.{
  Code,
  Coding
}
import de.dnpm.dip.model.Id
import de.dnpm.dip.mtb.model.Variant
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.coding.hgvs.HGVS


sealed trait GeneAlteration
{
  val variant: Id[Variant]
  val gene: Coding[HGNC]
}


object GeneAlteration
{

  object Type extends Enumeration
  {
    val SNV,CNV,Fusion = Value
  }


  final case class SNV
  (
    variant: Id[Variant],
    gene: Coding[HGNC],
    proteinChange: Option[Code[HGVS]]
  )
  extends GeneAlteration


  final case class CNV
  (
    variant: Id[Variant],
    gene: Coding[HGNC],
    `type`: CNV.Type.Value
  )
  extends GeneAlteration


  object CNV
  {
    object Type extends Enumeration
    {
      val Amplification, Deletion = Value
    }
  }


  final case class Fusion
  (
    variant: Id[Variant],
    gene: Coding[HGNC],
    partner: Coding[HGNC]
  )
  extends GeneAlteration



  // "Dummy" alteration type for use as base/parent of other alteration types
  final case class Unspecified
  (
    gene: Coding[HGNC]
  )
  extends GeneAlteration
  {
    val variant = Id("") // Dummy
  }

  def apply(gene: Coding[HGNC]): GeneAlteration =
    Unspecified(gene)

}
