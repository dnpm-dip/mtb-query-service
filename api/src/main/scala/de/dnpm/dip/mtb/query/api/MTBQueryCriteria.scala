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
import de.dnpm.dip.service.query.{
  PatientFilter,
  Query,
}
import de.dnpm.dip.mtb.model.{
  CNV,
  RECIST
}
import play.api.libs.json.{
  Json,
  JsPath,
  Format,
  Reads,
  Writes,
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

sealed trait VariantCriteria
{
  val supporting: Option[Boolean]
}

final case class SNVCriteria
(
  gene: Option[Coding[HGNC]],
  dnaChange: Option[Coding[HGVS.DNA]],
  proteinChange: Option[Coding[HGVS.Protein]],
  supporting: Option[Boolean] = None
)
extends VariantCriteria

final case class CNVCriteria
(
  affectedGenes: Option[Set[Coding[HGNC]]],
  `type`: Option[Coding[CNV.Type.Value]],
  supporting: Option[Boolean] = None
)
extends VariantCriteria

final case class FusionCriteria
(
  fusionPartner5pr: Option[Coding[HGNC]],
  fusionPartner3pr: Option[Coding[HGNC]],
  supporting: Option[Boolean] = None
)
extends VariantCriteria


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
  drugs: Set[Tree[Coding[Medications]]],
//  drugs: Set[Tree[Coding[ATC]]],
  usage: Option[Set[Coding[MedicationUsage.Value]]]
)

object MedicationCriteria
{
  import play.api.libs.functional.syntax._
  import scala.util.matching.Regex

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
  simpleVariants: Option[Set[SNVCriteria]],
  copyNumberVariants: Option[Set[CNVCriteria]],
  dnaFusions: Option[Set[FusionCriteria]],
  rnaFusions: Option[Set[FusionCriteria]],
  medication: Option[MedicationCriteria],
  responses: Option[Set[Coding[RECIST.Value]]]
)
{
  def getDiagnoses          = diagnoses.getOrElse(Set.empty)
  def getTumorMorphologies  = tumorMorphologies.getOrElse(Set.empty) 
  def getSimpleVariants     = simpleVariants.getOrElse(Set.empty)    
  def getCopyNumberVariants = copyNumberVariants.getOrElse(Set.empty)
  def getDnaFusions         = dnaFusions.getOrElse(Set.empty) 
  def getRnaFusions         = rnaFusions.getOrElse(Set.empty)        
  def getResponses          = responses.getOrElse(Set.empty)        
  def getDrugs              = medication.map(_.drugs).getOrElse(Set.empty)
}


object MTBQueryCriteria
{

  implicit val formatSNVCriteria: OFormat[SNVCriteria] =
    Json.format[SNVCriteria]

  implicit val formatCNVCriteria: OFormat[CNVCriteria] =
    Json.format[CNVCriteria]

  implicit val formatFusionCriteria: OFormat[FusionCriteria] =
    Json.format[FusionCriteria]

//  implicit val formatMedicationCriteria: OFormat[MedicationCriteria] =
//    Json.format[MedicationCriteria]

  implicit val format: OFormat[MTBQueryCriteria] =
    Json.format[MTBQueryCriteria]

}
