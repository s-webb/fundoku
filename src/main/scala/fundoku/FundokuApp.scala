package fundoku

import scala.scalajs.js.JSApp
import scala.scalajs.js.timers._

import org.scalajs.dom.document

import scalatags.JsDom.all._
import scalatags.JsDom.svgTags._

object FundokuApp extends JSApp {

  val colDims = Dims(9, 3, 30.0d, 0.10d, 0.20d)
  val rowDims = Dims(9, 3, 30.0d, 0.10d, 0.20d)
  val dims = (colDims, rowDims)

  val updateDelayMillis = 200

  val cellActivity = Array.fill(dims.count)(Activity.apply)

  var tick = 0
  var updateIndex = 0

  var currGridDom = Builders.grid(dims)(intensity).render

  def main(): Unit = {
    document.body.appendChild(currGridDom)
    setTimeout(updateDelayMillis) {
      redraw()
    }
  }

  def redraw(): Unit = {
    updateActivity()
    val newGridDom = Builders.grid(dims)(intensity).render
    document.body.replaceChild(newGridDom, currGridDom)
    currGridDom = newGridDom
    setTimeout(updateDelayMillis) {
      redraw()
    }
  }

  def toArrIndex(rowDims: Dims)(c: Int, r: Int): Int = (r * rowDims.count) + c

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

  def intensity(col: Int, row: Int) = 1.0d - cellActivity(toArrIndex(rowDims)(col, row)).normalizedAggValue
  // val colP = (col + row) / (colDims.count + rowDims.count).toDouble
}

object Builders {

  import Dims._
  import Color._
  import scalatags.JsDom.svgAttrs._

  def grid(dims: GridDims)(intensity: (Int, Int) => Double): Frag = {
    val cellFs: Seq[(GridDims, Int, Int) => Frag] = Seq(cell(intensity), cellText)
    svg(height := dims.rows.totalSize, width := dims.cols.totalSize) (
      background(dims) +: cellFs.flatMap(dims.mapCells)
    )
  }

  def background(dims: GridDims): Frag = {
    rect(
      x := 0.0d,
      y := 0.0d,
      width := dims.cols.totalSize,
      height := dims.rows.totalSize,
      fill := "black")
  }

  def cell(intensity: (Int, Int) => Double)(dims: GridDims, col: Int, row: Int): Frag = {
    val cellX = dims.cols.indexLoc(col)
    val cellY = dims.rows.indexLoc(row)
    val fillCol = white.blend(red, intensity(col, row))
    rect(
      x := cellX,
      y := cellY,
      width := dims.cols.size,
      height := dims.rows.size,
      fill := Color.toSvg(fillCol))
  }

  def cellText(dims: GridDims, col: Int, row: Int): Frag = {
    val textX = dims.cols.indexLoc(col) + (dims.cols.size * 0.5d)
    val thinRow = ((row + 1) % dims.rows.blockCount) != 0
    val thinRowMod = if (thinRow) dims.rows.thickPad * 0.5d else 0.0d
    val textY = dims.rows.indexLoc(row + 1) - (dims.rows.size * 0.5d) + thinRowMod
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

  import scala.language.implicitConversions

  type Color = (Int, Int, Int)

  val white: Color = (255, 255, 255)
  val red: Color = (255, 0, 0)

  def blend(col1: Color, col2: Color, p: Double): Color = {
    val mix: (Int, Int) => Int = (l, r) => ((l * p) + (r * (1 - p))).toInt
    // has to be a better way of doing this
    // (mix(col1._1, col2._1), mix(col1._2, col2._2), mix(col1._3, col2._3))

    // something in shapeless maybe?
    import shapeless._
    import poly._
    import syntax.std.tuple._
    object mixP extends Poly1 {
      implicit def caseInts = at[(Int, Int)](t => mix(t._1, t._2))
    }
    (col1 zip col2) map mixP
    // ... it took 2 hours to come up with that, shapeless ftw
  }

  def toSvg(color: Color): String = {
    val (r, g, b) = color
    s"rgb($r, $g, $b)"
  }

  implicit def toColorOps(col: Color): ColorOps = new ColorOps {
    override def toSvg = Color.toSvg(col)
    override def blend(other: Color, p: Double): Color = Color.blend(col, other, p)
  }

  trait ColorOps {
    def toSvg: String
    def blend(other: Color, p: Double): Color
  }
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

object Dims {

  import scala.language.implicitConversions

  type GridDims = (Dims, Dims)

  implicit def gridDimsOps(dims: GridDims): GridDimsOps = new GridDimsOps {
    override def cols = dims._1
    override def rows = dims._2
  }

  trait GridDimsOps {
    def cols: Dims
    def rows: Dims
    def count: Int = cols.count * rows.count
    def mapCells[A](f: (GridDims, Int, Int) => A): Seq[A] = {
      for { 
        c <- (0 until cols.count)
        r <- (0 until rows.count) 
      } yield f((cols, rows), c, r)
    }
  }
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
