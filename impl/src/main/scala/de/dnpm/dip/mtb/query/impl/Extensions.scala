package de.dnpm.dip.mtb.query.impl


import de.dnpm.dip.model.CarePlan
import CarePlan.BoardType.TherapyBoard
import de.dnpm.dip.mtb.model.{
  MTBCarePlan,
  MTBPatientRecord
}


object extensions
{


  implicit class MTBPatientRecordExtensions(val record: MTBPatientRecord) extends AnyVal
  {
   
    // Get therapy board plans as those with this declared board-type (or with recommendations if type undefined)
    def therapyBoardPlans: List[MTBCarePlan] =
      record.getCarePlans.filter(carePlan =>
        carePlan.boardType match {
          case Some(value) => value.code.enumValue == TherapyBoard
          case None        => carePlan.medicationRecommendations.exists(_.nonEmpty)
        }
      )

  }

}
