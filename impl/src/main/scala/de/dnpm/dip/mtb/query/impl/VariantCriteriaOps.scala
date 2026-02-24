package de.dnpm.dip.mtb.query.impl


import de.dnpm.dip.model.GeneAlterationReference
import de.dnpm.dip.mtb.model.Variant


object VariantCriteriaOps
{

  private type CanHaveSupportingVariants = {
    def supportingVariants: Option[List[GeneAlterationReference[Variant]]]
  }

  import scala.language.reflectiveCalls


  implicit class VariantOps(val variant: Variant) extends AnyVal
  {

    def isSupporting[T <: CanHaveSupportingVariants](
      implicit ts: Iterable[T]
    ): Boolean =
      ts.exists(
        _.supportingVariants.exists(
          _.exists(_.variant.id == variant.id)
        )
      )

  }

}
