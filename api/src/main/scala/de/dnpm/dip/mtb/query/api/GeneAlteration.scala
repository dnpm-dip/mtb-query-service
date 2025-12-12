package de.dnpm.dip.mtb.query.api


import de.dnpm.dip.coding.{
  Code,
  Coding
}
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.coding.hgvs.HGVS



sealed trait GeneAlteration
{
  val gene: Coding[HGNC]
}


object GeneAlteration
{

  final case class SNV
  (
    gene: Coding[HGNC],
    proteinChange: Option[Code[HGVS]]
  )
  extends GeneAlteration


  final case class CNV
  (
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


  def apply(gene: Coding[HGNC]): GeneAlteration =
    Unspecified(gene)

}
