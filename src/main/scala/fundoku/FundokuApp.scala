package fundoku

import scala.reflect.ClassTag

import scala.scalajs.js.JSApp
import scala.scalajs.js.timers._

import org.scalajs.dom.document
import org.scalajs.dom.raw.Node

import scalatags.JsDom.all._
import scalatags.JsDom.svgTags._

object FundokuApp extends JSApp {

  val colDims = Dims(9, 3, 30.0d, 0.10d, 0.20d)
  val rowDims = Dims(9, 3, 30.0d, 0.10d, 0.20d)
  val dims = (colDims, rowDims)

  val updateDelayMillis = 200
  val maxTickVal = 100
  val historySize = 5
  val updateWidth = 5

  val maxCellValue = Activity(List.fill(historySize)(maxTickVal)).aggregateValue
  val updateActivity = Activity.updateActivity(updateWidth, maxTickVal) _

  // Could probably combine these two vars into one, which would be the only mutable state
  var cellActivity = (0, Array.fill(dims.count)(Activity(List.fill(historySize)(0))))
  var currGridDom = renderGridDom

  def main(): Unit = {
    document.body.appendChild(currGridDom)
    scheduleRedraw()
  }

  def redraw(): Unit = {
    cellActivity = updateActivity(cellActivity)
    val newGridDom = renderGridDom
    document.body.replaceChild(newGridDom, currGridDom)
    currGridDom = newGridDom
    scheduleRedraw()
  }

  def scheduleRedraw(): Unit =
    setTimeout(updateDelayMillis) { redraw() }

  def renderGridDom: Node = Builders.grid(dims)(intensity(cellActivity._2)).render

  def toArrIndex(rowDims: Dims)(c: Int, r: Int): Int = (r * rowDims.count) + c

  def intensity(cells: Array[Activity])(col: Int, row: Int) = {
    val cellValue = cells(toArrIndex(rowDims)(col, row)).aggregateValue
    1.0d - (cellValue / maxCellValue)
  }
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
    // Not sure that this should depend on Color
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
      implicit def caseInts = at[(Int, Int)](mix.tupled)
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

  def apply(vs: Int*): Activity = Activity(vs.toList)

  def updateActivity(updateRangeSize: Int, maxTickVal: Int)(activity: (Int, Array[Activity])): (Int, Array[Activity]) = {
    val (updateIndex, cellActivity) = activity
    val maxIndex = cellActivity.size
    val (wrapped, indices) = indicesToUpdate(updateIndex, updateRangeSize, maxIndex)
    val tickVals = if (wrapped) Seq(maxTickVal, 0) else Seq(0, maxTickVal)
    val newActivity = updateSlices(cellActivity, indices, tickVals.map(n => (_:Activity).update(n)))
    val newIndex = addAndWrap(updateIndex, 1, maxIndex)
    (newIndex, newActivity)
  }

  def indicesToUpdate(updateStart: Int, rangeSize: Int, maxIndex: Int): (Boolean, Seq[Int]) = {
    val updateEnd = addAndWrap(updateStart, rangeSize, maxIndex)
    if (updateEnd < updateStart) {
      (true, Seq(0, updateEnd, updateStart, maxIndex))
    } else {
      (false, Seq(0, updateStart, updateEnd, maxIndex))
    }
  }

  // could this be generalized further to operate on anything that can be sliced? sounds tricky
  def updateSlices[A, B: ClassTag](cells: Array[A], boundaries: Seq[Int], fs: Seq[A => B]): Array[B] = {
    // Why is there no tupled version of sliding? bah
    val slices = boundaries.sliding(2)
    val updates = Iterator.continually(fs).flatten
    (slices zip updates).flatMap { case (is, f) => cells.slice(is(0), is(1)).map(f) }.toArray
  }

  // undefined for i >= maxIndex, or <= 0
  def addAndWrap(n: Int, i: Int, max: Int): Int = {
    val notWrapped = n + i
    if (notWrapped >= max) notWrapped - max else notWrapped
  }

  def easing(n: Int) = 1.0d / scala.math.pow(2, n)
}

// This doesn't need to be a case class, could just as easily be a tuple
case class Activity(ticks: List[Int]) {
  def update(newValue: Int): Activity = Activity(newValue :: ticks.dropRight(1))
  def aggregateValue: Double =
    ticks.zipWithIndex.map { case (t, n) => t * Activity.easing(n) }.sum
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
