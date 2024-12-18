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

/*
    object Type
    extends CodedEnum("dnpm-dip/mtb/query/gene-alteration/copy-number-type")
    with DefaultCodeSystem
    {
      val Amplification, Deletion = Value
 
      final class ProviderSPI extends CodeSystemProviderSPI
      {
        override def getInstance[F[_]]: CodeSystemProvider[Any,F,Applicative[F]] =
          new Provider.Facade[F]
      }
 
    }
*/
  }


  final case class Fusion
  (
    gene: Coding[HGNC],
    partner: Coding[HGNC]
  )
  extends GeneAlteration



  // "Dummy" alteration type for use as base/parent of other alteration types
  final case class Base
  (
    gene: Coding[HGNC]
  )
  extends GeneAlteration


  def apply(gene: Coding[HGNC]): GeneAlteration =
    Base(gene)

}
