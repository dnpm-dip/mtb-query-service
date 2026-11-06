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
import de.dnpm.dip.coding.icd.ICD10GM
import de.dnpm.dip.model.Medications
import de.dnpm.dip.mtb.model.{
  MTBDiagnosis,
  MTBMedicationRecommendation,
  MTBSystemicTherapy
}
import de.dnpm.dip.mtb.query.api.{
  DiagnosisFilter,
  RecommendationFilter,
  TherapyFilter
}



object MTBFilterExtensions
{


  implicit class DiagnosisFilterPredicate(val filter: DiagnosisFilter) extends AnyVal
  {

    /**
     * For the queried diagnosis Codings (expanded to tree),
     * check whether there exists a matching occurring diagnosis code
     */
    def apply(
      diagnoses: Iterable[MTBDiagnosis]
    )(
      implicit icd10gm: CodeSystemProvider[ICD10GM,Id,Applicative[Id]]
    ): Boolean =
      filter.code match {
        case Some(queriedCodings) if queriedCodings.nonEmpty =>
          queriedCodings.flatMap(_.expand)
            .exists(queriedCoding => diagnoses.exists(diagnosis => queriedCoding.exists(_.code == diagnosis.code.code)))

        // True by default if nothing to filter by
        case _ => true
      }

  }


  implicit class RecommendationFilterPredicate(val filter: RecommendationFilter) extends AnyVal
  {

    def apply(
      recommendations: Iterable[MTBMedicationRecommendation]
    )(
      implicit atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
    ): Boolean = 
      matches(
        filter.medication,
        recommendations.map(_.medication) 
      )

  }


  implicit class TherapyFilterPredicate(val filter: TherapyFilter) extends AnyVal
  {

    def apply(
      therapies: Iterable[MTBSystemicTherapy]
    )(
      implicit atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
    ): Boolean =
      matches(
        filter.medication,
        therapies.flatMap(_.medication.filter(_.nonEmpty))
      )
  }


  /**
   * For the queried medication combinations (expanded to tree),
   * check whether there exists an occurring medication combination
   * such that each entry in the queried combination has a match by name
   */
  private def matches(
    filteredMedications: Option[Set[Set[Coding[Medications]]]],
    occurringMedications: => Iterable[Set[Coding[Medications]]]
  )(
    implicit atc: CodeSystemProvider[ATC,Id,Applicative[Id]]
  ): Boolean =
    filteredMedications match { 
      case Some(medication) if medication.nonEmpty =>

        val occurringMedicationNames = occurringMedications.map(_.flatMap(_.display.map(_.toLowerCase)))

        expandedMedicationNames(medication).exists(
          filteredMedications => occurringMedicationNames.exists(
            names => filteredMedications.forall(entry => names.exists(entry.contains))
          )
        )

      // True by default if nothing to filter by
      case _ => true 
    }


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

}
