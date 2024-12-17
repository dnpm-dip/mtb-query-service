package de.dnpm.dip.mtb.query.api


import de.dnpm.dip.coding.Coding
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
    dnaChange: Option[Coding[HGVS]],
    proteinChange: Option[Coding[HGVS]]
  )
  extends GeneAlteration


  final case class CopyNumberVariation
  (
    gene: Coding[HGNC],
    `type`: CopyNumberVariation.Type.Value
  )
  extends GeneAlteration


  object CopyNumberVariation
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



}
