package de.dnpm.dip.mtb.query.api


import de.dnpm.dip.coding.{
  Code,
  Coding
}
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.coding.hgvs.HGVS
import play.api.libs.json.{
  Json,
  JsObject,
  Format,
  OWrites
}


sealed trait GeneAlteration
{
  val gene: Coding[HGNC]
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
    gene: Coding[HGNC],
    proteinChange: Option[Code[HGVS]]
  )
  extends GeneAlteration

  final case class CNV
  (
    gene: Coding[HGNC],
    copyNumberType: CNV.Type.Value
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
  final case class Unspecified(gene: Coding[HGNC]) extends GeneAlteration

  def apply(gene: Coding[HGNC]): GeneAlteration = Unspecified(gene)


  /**
   * Custom OWrites to append "type" attribute
   */ 
  implicit val writesSNV: OWrites[SNV] = 
    Json.writes[SNV].transform(
      (json: JsObject) => json +  ("type" -> Json.toJson(Type.SNV))
    )

  implicit val writesCNV: OWrites[CNV] =
    Json.writes[CNV].transform(
      (json: JsObject) => json +  ("type" -> Json.toJson(Type.CNV))
    )

  implicit val writesFusion: OWrites[Fusion] =
    Json.writes[Fusion].transform(
      (json: JsObject) => json +  ("type" -> Json.toJson(Type.Fusion))
    )

  implicit val writes: OWrites[GeneAlteration] =
    OWrites { 
      case snv: SNV          => Json.toJsObject(snv)
      case cnv: CNV          => Json.toJsObject(cnv)
      case fusion: Fusion    => Json.toJsObject(fusion)
      case Unspecified(gene) => Json.obj("gene" -> gene)
    }

}
