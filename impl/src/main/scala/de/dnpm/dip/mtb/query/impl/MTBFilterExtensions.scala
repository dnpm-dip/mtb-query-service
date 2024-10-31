package de.dnpm.dip.mtb.query.impl


import cats.{
  Applicative,
  Id
}
import de.dnpm.dip.util.Tree
import de.dnpm.dip.coding.{
  Coding,
  CodeSystemProvider
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.UnregisteredMedication
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.service.query.PatientFilter
import de.dnpm.dip.model.Medications
import de.dnpm.dip.mtb.model.{
  MTBPatientRecord,
  MTBDiagnosis,
  MTBMedicationRecommendation,
  MTBMedicationTherapy
}
import de.dnpm.dip.mtb.query.api.{
  MTBFilters,
  DiagnosisFilter,
  RecommendationFilter,
  TherapyFilter
}



object MTBFilterExtensions
{

  private def expandedMedicationNames(
    meds: Set[Set[Coding[Medications]]]
  )(
    implicit atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
  ): Set[Set[Tree[String]]] =
    meds.map(
      _.flatMap(
        coding => coding.system match {
          case sys if sys == Coding.System[ATC].uri =>
            coding.asInstanceOf[Coding[ATC]]
              .expand
              .map(_.map(_.display.get.toLowerCase))

          case _ => Some(Tree(coding.code.value.toLowerCase))
        }
      )
    )


  implicit class DiagnosisPredicate(val filter: DiagnosisFilter) extends AnyVal
  {

    def apply(
      diagnoses: Iterable[MTBDiagnosis]
    )(
      implicit icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]]
    ): Boolean = {

      val occurringCodes =
        diagnoses.map(_.code.code)

      filter
        .code
        .map(_.flatMap(_.expand))
        .map(codes => occurringCodes exists (code => codes exists (_ exists (_.code == code))))
        .getOrElse(true)
    }

  }


  implicit class RecommendationPredicate(val filter: RecommendationFilter) extends AnyVal
  {

    def apply(
      recommendations: Iterable[MTBMedicationRecommendation]
    )(
      implicit atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
    ): Boolean = {

      val occurringDrugNames =
        recommendations
          .map(_.medication.flatMap(_.display).map(_.toLowerCase))

      val occurring: String => Boolean =
        name => occurringDrugNames exists (_ contains name)
          
      filter
        .medication
        .map(expandedMedicationNames)
        .map(_ exists (_ forall (_ exists occurring)))
        .getOrElse(true)
    }
  }


  implicit class TherapyPredicate(val filter: TherapyFilter) extends AnyVal
  {

    def apply(
      therapies: Iterable[MTBMedicationTherapy]
    )(
      implicit atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
    ): Boolean = {

      val optOccurringDrugNames =
        therapies.map(_.medication.map(_.flatMap(_.display).map(_.toLowerCase)))
      
      val occurring: String => Boolean =
        name => optOccurringDrugNames.exists(_ exists (_ contains name))
            
      filter
        .medication
        .map(expandedMedicationNames)
        .map(_ exists (_ forall (_ exists occurring)))
        .getOrElse(true)
    }
  }

}
