package de.dnpm.dip.mtb.query.impl



import de.dnpm.dip.util.Completer
import de.dnpm.dip.coding.{
  Coding,
  CodeSystem,
  CodeSystemProvider
}
import de.dnpm.dip.coding.atc.ATC
import de.dnpm.dip.coding.hgnc.HGNC
import de.dnpm.dip.model.{
  Patient,
  Site
}
import de.dnpm.dip.mtb.model._
import de.dnpm.dip.mtb.query.api._



trait Completers
{


  import Completer.syntax._


  val localSite: Coding[Site]

//  implicit val hgnc: CodeSystem[HGNC]

//  implicit val atc: CodeSystemProvider[ATC]


  implicit val patientCompleter: Completer[Patient] =
    Completer.of(
      pat =>
        pat.copy(
          gender       = pat.gender.complete,
          managingSite = Some(localSite)
        )
    )


  val CriteriaExpander: Completer[MTBQueryCriteria] = ???


  implicit val mtbPatientRecordCompleter: Completer[MTBPatientRecord] = ???

  implicit protected val criteriaCompleter: Completer[MTBQueryCriteria] = ???

}
