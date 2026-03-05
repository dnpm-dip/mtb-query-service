package de.dnpm.dip.mtb.query.api


import de.dnpm.dip.coding.{
  Code,
  Coding
}
import de.dnpm.dip.model.Id
import de.dnpm.dip.mtb.model.Variant
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.coding.hgvs.HGVS
import play.api.libs.json.{
  Json,
  Format,
  OWrites
}


sealed trait GeneAlteration
{
  val variant: Id[Variant]
  val gene: Coding[HGNC]


  import GeneAlteration.{SNV,CNV,Fusion,Unspecified}

  /**
   * Override equals to exclude "variant" from comparison: This is used for internal reference only,
   * but 2 GeneAlteration instance of the same type should be equal if their actual attributes are equal,
   * irrespective of the Variant object they stem from
   */ 
  override def equals(that: Any): Boolean =
    (this,that) match {
      case (left: SNV,right: SNV)                  => (left.gene,left.proteinChange) == (right.gene,right.proteinChange)
      case (left: CNV,right: CNV)                  => (left.gene,left.`type`) == (right.gene,right.`type`)
      case (left: Fusion,right: Fusion)            => (left.gene,left.partner) == (right.gene,right.partner)
      case (left: Unspecified, right: Unspecified) => left.gene == right.gene
      case _ => false
    }

  /**
   * Override hashCode to exclude "variant" (see above for equals)
   */ 
  override def hashCode: Int =
    this match {
      case SNV(_,gene,proteinChange) => (gene,proteinChange).hashCode
      case CNV(_,gene,typ) => (gene,typ).hashCode 
      case Fusion(_,gene,partner) => (gene,partner).hashCode
      case Unspecified(gene) => gene.hashCode
    }
}




object GeneAlteration
{

  object Type extends Enumeration
  {
    val SNV,CNV,Fusion = Value

    implicit val format: Format[Value] = Json.formatEnum(this)
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

  def apply(gene: Coding[HGNC]): GeneAlteration = Unspecified(gene)


  /**
   * Custom OWrites required:
   * - append "type" attribute
   * - ignore GeneAlteration.variant because only internally relevant
   */ 
  implicit val writesSNV: OWrites[SNV] = 
    OWrites(
      snv => Json.obj(
        "type" -> Type.SNV,
        "gene" -> snv.gene,
        "proteinChange" -> snv.proteinChange
      )
    )

  implicit val writesCNV: OWrites[CNV] =
    OWrites(
      cnv => Json.obj(
        "type" -> Type.CNV,
        "gene" -> cnv.gene,
        "cnvType" -> cnv.`type`
      )
    )

  implicit val writesFusion: OWrites[Fusion] =
    OWrites(
      fusion => Json.obj(
        "type" -> Type.Fusion,
        "gene" -> fusion.gene,
        "partner" -> fusion.partner
      )
    )

  implicit val writes: OWrites[GeneAlteration] =
    OWrites { 
      case snv: SNV          => Json.toJsObject(snv)
      case cnv: CNV          => Json.toJsObject(cnv)
      case fusion: Fusion    => Json.toJsObject(fusion)
      case Unspecified(gene) => Json.obj("gene" -> gene)
    }

}
