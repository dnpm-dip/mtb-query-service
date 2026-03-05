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


// --------------------------------------------------------

final case class GeneAlterationCriteria
(
  gene: Coding[HGNC],
  variant: Option[GeneAlterationCriteria.OnVariant] = None,
  supporting: Option[Boolean] = None,
  wildtype: Option[Boolean] = None
)

object GeneAlterationCriteria
{

  sealed abstract class OnVariant

  final case class OnSNV 
  (
    dnaChange: Option[Code[HGVS.DNA]],
    proteinChange: Option[Code[HGVS.Protein]]
  )
  extends OnVariant

  final case class OnCNV
  (
    copyNumberType: Option[Set[Coding[CNV.Type.Value]]],
  )
  extends OnVariant

  final case class OnFusion
  (
    partner: Option[Coding[HGNC]]
  )
  extends OnVariant

  implicit val formatGeneAlterationType: Format[GeneAlteration.Type.Value] =
    Json.formatEnum(GeneAlteration.Type)

  implicit val formatOnSNV: OFormat[OnSNV] =
    Json.format[OnSNV]

  implicit val formatOnCNV: OFormat[OnCNV] =
    Json.format[OnCNV]

  implicit val formatOnFusion: OFormat[OnFusion] =
    Json.format[OnFusion]


  implicit val readsOnVariant: Reads[OnVariant] =
    Reads(
      js => (js \ "type").validate[GeneAlteration.Type.Value].flatMap {
        case GeneAlteration.Type.SNV    => Json.fromJson[OnSNV](js)
        case GeneAlteration.Type.CNV    => Json.fromJson[OnCNV](js) 
        case GeneAlteration.Type.Fusion => Json.fromJson[OnFusion](js)
      }
    )

  implicit val writesOnVariant: OWrites[OnVariant] =
   OWrites {
     case snv: OnSNV       => Json.toJsObject(snv)    + ("type" -> Json.toJson(GeneAlteration.Type.SNV))
     case cnv: OnCNV       => Json.toJsObject(cnv)    + ("type" -> Json.toJson(GeneAlteration.Type.CNV))
     case fusion: OnFusion => Json.toJsObject(fusion) + ("type" -> Json.toJson(GeneAlteration.Type.Fusion))
   }

  implicit val format: OFormat[GeneAlterationCriteria] =
    Json.format[GeneAlterationCriteria]

}


final case class GeneAlterations
(
  items: Set[GeneAlterationCriteria],
  operator: Option[LogicalOperator.Value] = None,
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
  diagnoses: Option[Set[Coding[ICD10GM]]] = None,
  tumorMorphologies: Option[Set[Coding[ICDO3.M]]] = None,
  geneAlterations: Option[GeneAlterations] = None,
  medication: Option[MedicationCriteria] = None,
  responses: Option[Set[Coding[RECIST.Value]]] = None
)


object MTBQueryCriteria
{
  implicit val format: OFormat[MTBQueryCriteria] =
    Json.format[MTBQueryCriteria]
}
