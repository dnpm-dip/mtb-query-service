package de.dnpm.dip.mtb.query.impl


import java.time.Instant
import java.util.UUID.randomUUID
import java.time.temporal.ChronoUnit.DAYS
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.Inspectors._
import de.dnpm.dip.model.ClosedInterval
import de.dnpm.dip.mtb.query.api.KaplanMeier.DataPoint


class KaplanMeierTests extends AnyFlatSpec
{

  import scala.util.chaining._

//  implicit val rnd = new scala.util.Random(42)

  // Ref. data from Julia Grafs bachelor thesis (p. 9)
  val refData0 =
    Seq(
      28L -> true,
      40L -> false,
      26L -> false,
      8L  -> true,
      34L -> false,
      12L -> true,
      23L -> false,
      34L -> true,
      4L  -> false,
      28L -> true
    )

  val refResults0 =
    Seq(
      DataPoint(0L, 1.00,false,ClosedInterval(1.00,1.00)),
      DataPoint(4L, 1.00,true, ClosedInterval(1.00,1.00)),
      DataPoint(8L, 0.89,false,ClosedInterval(0.69,1.09)),
      DataPoint(12L,0.78,false,ClosedInterval(0.50,1.06)),
    )


  /*
   Reference data source:
    
   Dtsch Med Wochenschr 2007; 132: e36–e38 · A. Ziegler et al.,
   Überlebenszeitanalyse: Eigenschaften und Kaplan-Meier Methode'
  */
  val refData1 = 
    Seq(
      1L   -> true,
      3L   -> true,
      4L   -> true,
      5L   -> true,
      5L   -> true,
      8L   -> true,
      12L  -> true,
      13L  -> true,
      18L  -> true,
      23L  -> true,
      26L  -> true,
      27L  -> true,
      30L  -> true,
      42L  -> true,
      56L  -> true,
      62L  -> true,
      69L  -> true,
      104L -> true,
      104L -> true,
      112L -> true,
      129L -> true,
      181L -> true,
      8L   -> false,
      67L  -> false,
      76L  -> false,
      104L -> false,
      176L -> false,
      231L -> false
    ) 

  // Reference data origin: Python Lifelines
  val refResults1 = 
    Seq(
      1.000000,
      0.964286,
      0.928571,
      0.892857,
      0.821429,
      0.785714,
      0.748299,
      0.710884,
      0.673469,
      0.636054,
      0.598639,
      0.561224,
      0.523810,
      0.486395,
      0.448980,
      0.411565,
      0.411565,
      0.370408,
      0.370408,
      0.277806,
      0.222245,
      0.166684,
      0.166684,
      0.083342,
      0.083342
    )

  /*
   Reference data source:
    
   Rich JT, Neely JG, Paniello RC, Voelker CC, Nussenbaum B, Wang EW.
   A practical guide to understanding Kaplan-Meier curves.
   Otolaryngol Head Neck Surg. 2010 Sep;143(3):331-6.
   doi: 10.1016/j.otohns.2010.05.007. PMID: 20723767; PMCID: PMC3932959.

   https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3932959/
  */

  val refData2 = 
    Map(
      "Group1" -> Seq(
        100L -> true,
        200L -> true,
        300L -> true,
        400L -> true,
        450L -> true,
        500L -> false
      ),
      "Group2" -> Seq(
        50L  -> true,
        75L  -> true,
        100L -> true,
        150L -> false,
        200L -> true,
        350L -> true

      )
    )
 
  val refResults2 = 
    Map(
      "Group1" -> Seq(
        1.000,
        0.833,
        0.667,
        0.500,
        0.333,
        0.167,
      ),
      "Group2" -> Seq(
        1.000,
        0.833,
        0.667,
        0.500,
        0.500,
        0.250,
        0.000,
      )
    )
 

  private def roundOff(decimals: Int): Double => Double =
    if (decimals > 0)
      BigDecimal(_).setScale(decimals,BigDecimal.RoundingMode.HALF_UP).toDouble
    else
      identity

  private def checkResults(
    vals: Seq[Double],
    refVals: Seq[Double],
    decimals: Int = 0
  ): Unit = {
    forAll(
      refVals zip vals.map(roundOff(decimals))
    ){ 
      case (ref,v) => ref must equal (v)
    }

  }


  "DefaultKaplanMeierEstimator" must "have returned correct results for reference Data No. 0" in {

    val data =
      DefaultKaplanMeierEstimator(refData0)

    forAll(data zip refResults0){
      case (point,refPoint) =>
        roundOff(2)(point.survRate) must equal (refPoint.survRate)
    }

  }


  it must "have returned correct results for reference Data No. 1" in {

    checkResults(
      DefaultKaplanMeierEstimator(refData1).map(_.survRate),
      refResults1,
      6
    )
  }


  it must "have returned correct results for reference Data No. 2" in {

    refData2
      .foreach {
        case (group,data) =>
          checkResults(
            DefaultKaplanMeierEstimator(data).map(_.survRate),
            refResults2(group),
            3
          )
      }

  }


/*  
  it must "have returned correct results for MTBFile cohort" in {

    val mtbfiles =
      List.fill(100)(Gen.of[MTBFile].next)
        .map(
          Snapshot(
            Snapshot.Id(randomUUID.toString),
            Instant.now,
            _
          )
        )

    KaplanMeierModule.Projector
      .overallSurvival(mtbfiles,DAYS)
      .map {
        case (grp,data) =>
          grp -> KaplanMeierModule.Estimator(data).map(_._2)
      }
      .foreach {
        case (_,st) =>

          // Check that all values are in interval [0.0,1.0]
          forAll(st){ _ must be (0.5 +- 0.5) }

          // Check that value series is monotonically falling
          forAll(st.sliding(2).toSeq){
            l => l.last must be <= l.head
          }
      }
  }
*/

}
