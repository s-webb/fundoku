package fundoku

import scala.scalajs.js.JSApp
import scala.scalajs.js.timers._

import org.scalajs.dom.document

import scalatags.JsDom.all._
import scalatags.JsDom.svgTags._

object FundokuApp extends JSApp {

  val rowDims = Dims(9, 3, 30.0d, 0.10d, 0.20d)
  val colDims = Dims(9, 3, 30.0d, 0.10d, 0.20d)

  val updateDelayMillis = 200

  val cellActivity = Array.fill(81)(Activity.apply)

  var tick = 0
  var updateIndex = 0

  var currGridDom = mkGrid.render

  def main(): Unit = {
    document.body.appendChild(currGridDom)
    setTimeout(updateDelayMillis) {
      redraw()
    }
  }

  def redraw(): Unit = {
    updateActivity()
    val newGridDom = mkGrid.render
    document.body.replaceChild(newGridDom, currGridDom)
    currGridDom = newGridDom
    setTimeout(updateDelayMillis) {
      redraw()
    }
  }

  def toArrIndex(c: Int, r: Int): Int = (r * rowDims.count) + c

  // simulate an algorithm that accesses one cell 10 times in a tick,
  // then moves on to the next cell
  def updateActivity() = {
    def wrappedAdd(n: Int, i: Int): Int = {
      val unwrapped = n + i
      val wrapBy = unwrapped - cellActivity.size
      if (wrapBy >= 0) wrapBy else unwrapped
    }
    (0 until cellActivity.size).foreach { n =>
      if (n == updateIndex) {
        (0 until Activity.numTicksRemembered).foreach { m =>
          val wrappedM = wrappedAdd(n, m)
          cellActivity(wrappedM) = cellActivity(wrappedM).update(Activity.maxTickVal)
        }
      } else if (!(n > updateIndex && n < (updateIndex + Activity.numTicksRemembered))) {
        // Ignore anything that would have been updated by previous clause
        // god this is awful
        cellActivity(n) = cellActivity(n).update(0)
      }
    }
    updateIndex = wrappedAdd(updateIndex, 1)
  }

  def mkGrid: Frag = {
    import scalatags.JsDom.svgAttrs._
    val cells =
      for {
        r <- (0 until rowDims.count)
        c <- (0 until colDims.count)
      } yield Seq(mkCell(c, r), mkCellText(c, r))

    val bg = rect(
      x := 0.0d,
      y := 0.0d,
      width := colDims.totalSize,
      height := rowDims.totalSize,
      fill := "black")

    svg(height := rowDims.totalSize, width := colDims.totalSize) (
      bg +: cells.flatten
    )
  }

  def mkCell(col: Int, row: Int): Frag = {
    import scalatags.JsDom.svgAttrs._
    import Color._
    val cellX = colDims.indexLoc(col)
    val cellY = rowDims.indexLoc(row)
    // val colP = (col + row) / (colDims.count + rowDims.count).toDouble
    val intensity = 1.0d - cellActivity(toArrIndex(col, row)).normalizedAggValue
    val fillCol = blend(white, red, intensity)
    rect(
      x := cellX,
      y := cellY,
      width := colDims.size,
      height := rowDims.size,
      fill := fillCol.toSvg)
  }

  def mkCellText(col: Int, row: Int): Frag = {
    import scalatags.JsDom.svgAttrs._
    val textX = colDims.indexLoc(col) + (colDims.size * 0.5d)
    val thinRow = ((row + 1) % rowDims.blockCount) != 0
    val thinRowMod = if (thinRow) rowDims.thickPad * 0.5d else 0.0d
    val textY = rowDims.indexLoc(row + 1) - (rowDims.size * 0.5d) + thinRowMod
    val cellText = if (thinRow) "1" else "0"
    text(
      x := textX, 
      y := textY, 
      dominantBaseline := "center",
      textAnchor := "middle"
    )(cellText)
  }
}

object Color {

  def blend(col1: Color, col2: Color, p: Double): Color = {
    def blendEl(el: Color => Int): Int =
      ((el(col1) * p) + (el(col2) * (1 - p))).toInt
    Color(blendEl(_.r), blendEl(_.g), blendEl(_.b))
  }

  val white = Color(255, 255, 255)
  val red = Color(255, 0, 0)
}

object Activity {
  val numTicksRemembered = 5
  val maxTickVal = 10
  def apply: Activity = Activity(Seq.fill(numTicksRemembered)(0))
  def easing(n: Int) = 1.0d / scala.math.pow(2, n)
  val maxVal = Activity(Seq.fill(numTicksRemembered)(maxTickVal)).aggregateValue
  def normalize(v: Double): Double = v / maxVal
}

case class Activity(ticks: Seq[Int]) {
  import Activity._
  def update(tickCount: Int): Activity = Activity(tickCount +: ticks.dropRight(1))
  def aggregateValue: Double =
    ticks.zipWithIndex.map { case (t, n) => t * easing(n) }.sum
  def normalizedAggValue: Double = normalize(aggregateValue)
}

case class Color(r: Int, g: Int, b: Int) {
  def toSvg: String = s"rgb($r, $g, $b)"
}

case class Dims(
  count: Int,
  blockCount: Int,
  size: Double,
  padFactor: Double,
  thickPadFactor: Double
) {
  val pad = size * padFactor
  val thickPad = size * thickPadFactor

  val totalSize = indexLoc(count)

  def indexLoc(n: Int): Double =
    (n * size) + (numThickPadsAt(n) * thickPad) + (numThinPadsAt(n) * pad)

  private def numThickPadsAt(n: Int): Int = (n / blockCount) + 1
  private def numThinPadsAt(n: Int): Int = n - (n / blockCount)
}
