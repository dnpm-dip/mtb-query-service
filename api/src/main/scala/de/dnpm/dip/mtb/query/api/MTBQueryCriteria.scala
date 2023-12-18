package de.dnpm.dip.mtb.query.api


import cats.Applicative
import de.dnpm.dip.coding.{
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
  Format,
  Reads,
  Writes,
  OFormat,
  JsString
}



sealed trait LogicalOperator
object LogicalOperator
{

  final case object And extends LogicalOperator
  final case object Or extends LogicalOperator

  implicit val reads: Reads[LogicalOperator] =
    Reads(
      _.validate[String]
        .map(_.toLowerCase)
        .map {
          case "and" => And
          case "or"  => Or
        }
    )

  implicit val writes: Writes[LogicalOperator] =
    Writes { 
      case And => JsString("and")
      case Or  => JsString("or")
    }
}



final case class SNVCriteria
(
  gene: Option[Coding[HGNC]],
  dnaChange: Option[Coding[HGVS]],
  proteinChange: Option[Coding[HGVS]],
)


final case class CNVCriteria
(
  affectedGenes: Option[Set[Coding[HGNC]]],
  `type`: Option[Coding[CNV.Type.Value]]
)

final case class FusionCriteria
(
  fusionPartner5pr: Option[Coding[HGNC]],
  fusionPartner3pr: Option[Coding[HGNC]]
)


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
  operator: Option[LogicalOperator],
  medication: Set[Coding[ATC]],
  usage: Set[Coding[MedicationUsage.Value]]
)


final case class MTBQueryCriteria
(
  diagnoses: Option[Set[Coding[ICD10GM]]],
  tumorMorphologies: Option[Set[Coding[ICDO3.M]]],
  simpleVariants: Option[Set[SNVCriteria]],
  copyNumberVariants: Option[Set[CNVCriteria]],
  dnaFusions: Option[Set[FusionCriteria]],
  rnaFusions: Option[Set[FusionCriteria]],
  medications: Option[MedicationCriteria],
  responses: Option[Set[Coding[RECIST.Value]]]
)
{
  def getDiagnoses          = diagnoses.getOrElse(Set.empty)
  def getTumorMorphologies  = tumorMorphologies.getOrElse(Set.empty) 
  def getSimpleVariants     = simpleVariants.getOrElse(Set.empty)    
  def getCopyNumberVariants = copyNumberVariants.getOrElse(Set.empty)
  def getDnaFusions         = dnaFusions.getOrElse(Set.empty) 
  def getRnaFusions         = rnaFusions.getOrElse(Set.empty)        
  def geResponses           = responses.getOrElse(Set.empty)        
}


object MTBQueryCriteria
{

  implicit val formatSNVCriteria: OFormat[SNVCriteria] =
    Json.format[SNVCriteria]

  implicit val formatCNVCriteria: OFormat[CNVCriteria] =
    Json.format[CNVCriteria]

  implicit val formatFusionCriteria: OFormat[FusionCriteria] =
    Json.format[FusionCriteria]

  implicit val formatMedicationCriteria: OFormat[MedicationCriteria] =
    Json.format[MedicationCriteria]

  implicit val format: OFormat[MTBQueryCriteria] =
    Json.format[MTBQueryCriteria]

}
