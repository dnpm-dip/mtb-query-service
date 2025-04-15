package de.dnpm.dip.mtb.query.api


import java.net.URI
import cats.Applicative
import de.dnpm.dip.util.Tree
import de.dnpm.dip.coding.{
  Code,
  Coding,
  CodedEnum,
  CodeSystemProvider,
  CodeSystemProviderSPI,
  DefaultCodeSystem
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.coding.icd.ICDO3
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.coding.hgvs.HGVS
import de.dnpm.dip.coding.UnregisteredMedication
import de.dnpm.dip.model.Medications
import de.dnpm.dip.mtb.model.{
  CNV,
  RECIST
}
import play.api.libs.json.{
  Json,
  JsPath,
  Format,
  Reads,
  OWrites,
  OFormat,
  JsString
}



object LogicalOperator extends Enumeration
{
  val And = Value("and")
  val Or  = Value("or")

  implicit val format: Format[Value] =
    Json.formatEnum(this)
}


sealed trait CombinableItems[T]
{
  val operator: Option[LogicalOperator.Value]
  val items: Set[T]
}


sealed trait Negatable
{
  val negated: Option[Boolean]

/*
  def asNegated(cond: Boolean) =
    if (negated.getOrElse(false)) !cond
    else cond
*/

  def asNegated(cond: Boolean) =
    negated.map {
      case true  => !cond
      case false => cond
    }
    .getOrElse(cond)
}


sealed trait Supportable
{
  val supporting: Option[Boolean]
}


final case class SNVCriteria
(
  gene: Option[Coding[HGNC]],
  dnaChange: Option[Code[HGVS.DNA]],
  proteinChange: Option[Code[HGVS.Protein]],
  supporting: Option[Boolean] = None,
  negated: Option[Boolean] = None
)
extends Supportable
with Negatable


final case class CNVCriteria
(
  affectedGenes: Option[Set[Coding[HGNC]]],
  `type`: Option[Coding[CNV.Type.Value]],
  supporting: Option[Boolean] = None,
  negated: Option[Boolean] = None
)
extends Supportable
with Negatable


final case class FusionCriteria
(
  fusionPartner5pr: Option[Coding[HGNC]],
  fusionPartner3pr: Option[Coding[HGNC]],
  supporting: Option[Boolean] = None,
  negated: Option[Boolean] = None
)
extends Supportable
with Negatable


final case class VariantCriteria
(
  operator: Option[LogicalOperator.Value],
  simpleVariants: Option[Set[SNVCriteria]],
  copyNumberVariants: Option[Set[CNVCriteria]],
  dnaFusions: Option[Set[FusionCriteria]],
  rnaFusions: Option[Set[FusionCriteria]],
) 



// --------------------------------------------------------

final case class GeneAlterationCriteria
(
  gene: Coding[HGNC],
  variant: Option[GeneAlterationCriteria.VariantCriteria],
  supporting: Option[Boolean] = None,
  negated: Option[Boolean] = None
)
extends Supportable
with Negatable

object GeneAlterationCriteria
{

  sealed abstract class VariantCriteria

  final case class SNVCriteria 
  (
    dnaChange: Option[Code[HGVS.DNA]],
    proteinChange: Option[Code[HGVS.Protein]]
  )
  extends VariantCriteria

  final case class CNVCriteria
  (
    copyNumberType: Option[Set[Coding[CNV.Type.Value]]],
  )
  extends VariantCriteria

  final case class FusionCriteria
  (
    partner: Option[Coding[HGNC]]
  )
  extends VariantCriteria


  implicit val formatSNVCriteria: OFormat[SNVCriteria] =
    Json.format[SNVCriteria]

  implicit val formatCNVCriteria: OFormat[CNVCriteria] =
    Json.format[CNVCriteria]

  implicit val formatFusionCriteria: OFormat[FusionCriteria] =
    Json.format[FusionCriteria]

  implicit val readsVariantCriteria: Reads[VariantCriteria] =
    Reads(
      js => (js \ "type").validate[String].flatMap {
        case "SNV"    => Json.fromJson[SNVCriteria](js)
        case "CNV"    => Json.fromJson[CNVCriteria](js) 
        case "Fusion" => Json.fromJson[FusionCriteria](js)
      }
    )

  implicit val writesVariantCriteria: OWrites[VariantCriteria] =
   OWrites {
     case snv: SNVCriteria       => Json.toJsObject(snv)    + ("type" -> JsString("SNV"))
     case cnv: CNVCriteria       => Json.toJsObject(cnv)    + ("type" -> JsString("CNV"))
     case fusion: FusionCriteria => Json.toJsObject(fusion) + ("type" -> JsString("Fusion"))
   }
  

  implicit val format: OFormat[GeneAlterationCriteria] =
    Json.format[GeneAlterationCriteria]

}


final case class GeneAlterations
(
  operator: Option[LogicalOperator.Value],
  items: Set[GeneAlterationCriteria]
)
extends CombinableItems[GeneAlterationCriteria]

object GeneAlterations
{
  implicit val format: OFormat[GeneAlterations] =
    Json.format[GeneAlterations]
}

// --------------------------------------------------------


object MedicationUsage
extends CodedEnum("dnpm-dip/mtb/query/medication-usage")
with DefaultCodeSystem
{
  val Recommended = Value("recommended")
  val Used        = Value("used")

  override val display =
    Map(
      Recommended -> "Empfohlen",
      Used        -> "Verabreicht"
    )

  final class ProviderSPI extends CodeSystemProviderSPI
  {
    override def getInstance[F[_]]: CodeSystemProvider[Any,F,Applicative[F]] =
      new Provider.Facade[F]
  }

}



final case class MedicationCriteria
(
  operator: Option[LogicalOperator.Value],
  items: Set[Coding[Medications]],
  usage: Option[Set[Coding[MedicationUsage.Value]]]
)
extends CombinableItems[Coding[Medications]]
{
  var expandedDrugs: Set[Tree[Coding[Medications]]] =
    Set.empty
}

object MedicationCriteria
{

  import play.api.libs.functional.syntax._

  val atc = "(?i)atc".r.unanchored

  // Custom JSON Reads for Coding[Medications] to handle the "system" attribute as optional:
  // If specified as ATC, handle accordingly, else handle as "unregistered medication Coding"
  implicit val readsMedicationCoding: Reads[Coding[Medications]] =
    (
      (JsPath \ "code").read[Code[Medications]] and
      (JsPath \ "display").readNullable[String] and
      (JsPath \ "system").readNullable[URI]     and
      (JsPath \ "version").readNullable[String]
    )(
      (code,display,system,version) =>
        Coding[Medications](
          code,
          display,
          system match {
            case Some(uri) if atc matches uri.toString => Coding.System[ATC].uri
            case _ => Coding.System[UnregisteredMedication].uri
          },
          version
        )
    )

  implicit val formatMedicationCriteria: OFormat[MedicationCriteria] =
    Json.format[MedicationCriteria]
}



final case class MTBQueryCriteria
(
  diagnoses: Option[Set[Coding[ICD10GM]]],
  tumorMorphologies: Option[Set[Coding[ICDO3.M]]],
  geneAlterations: Option[GeneAlterations],
  variants: Option[VariantCriteria],
  medication: Option[MedicationCriteria],
  responses: Option[Set[Coding[RECIST.Value]]]
)


object MTBQueryCriteria
{

  implicit val formatSNVCriteria: OFormat[SNVCriteria] =
    Json.format[SNVCriteria]

  implicit val formatCNVCriteria: OFormat[CNVCriteria] =
    Json.format[CNVCriteria]

  implicit val formatFusionCriteria: OFormat[FusionCriteria] =
    Json.format[FusionCriteria]

  implicit val formatVariantCriteria: OFormat[VariantCriteria] =
    Json.format[VariantCriteria]

  implicit val format: OFormat[MTBQueryCriteria] =
    Json.format[MTBQueryCriteria]

}
