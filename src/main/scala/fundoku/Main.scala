package fundoku

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.collection.JavaConverters._

case class Cell(candidates: Seq[Int]) {

  override def toString: String =
    if (candidates.length == 1) candidates(0).toString else "."
}

object Cell {

  def apply(c: Char): Cell = c match {
    case '0' => new Cell(0 to 9)
    case n => new Cell(Seq(n.toString.toInt))
  }
}

case class Unit(cells: Seq[Cell])

case class Puzzle(name: String, units: Seq[Unit]) {

  override def toString: String = {
    s"Puzzle: $name\n" +
    "-------------\n" + 
    "|" +
    units(0).cells(0).toString +
    units(0).cells(1).toString +
    units(0).cells(2).toString +
    "|" +
    units(1).cells(0).toString +
    units(1).cells(1).toString +
    units(1).cells(2).toString +
    "|" +
    units(2).cells(0).toString +
    units(2).cells(1).toString +
    units(2).cells(2).toString +
    "|\n" +
    "|" +
    units(0).cells(3).toString +
    units(0).cells(4).toString +
    units(0).cells(5).toString +
    "|" +
    units(1).cells(3).toString +
    units(1).cells(4).toString +
    units(1).cells(5).toString +
    "|" +
    units(2).cells(3).toString +
    units(2).cells(4).toString +
    units(2).cells(5).toString +
    "|\n" +
    "|" +
    units(0).cells(6).toString +
    units(0).cells(7).toString +
    units(0).cells(8).toString +
    "|" +
    units(1).cells(6).toString +
    units(1).cells(7).toString +
    units(1).cells(8).toString +
    "|" +
    units(2).cells(6).toString +
    units(2).cells(7).toString +
    units(2).cells(8).toString +
    "|\n" +
    "-------------\n"
  }
}

object Main extends App {

  // Read each line of the file
  val path = Paths.get("src/main/resources/p096_sudoku.txt")
  val lines = Files.readAllLines(path, StandardCharsets.UTF_8).asScala

  val puzzles: Seq[Puzzle] = lines.grouped(10).map { puzzleLines =>
    val name = puzzleLines(0)
    val units: Seq[Unit] = puzzleLines.drop(1).grouped(3).flatMap { unitGroup =>
      val unitCells: Seq[(Seq[Cell], Seq[Cell], Seq[Cell])] = unitGroup.map { l =>
        val cells = l.map(Cell.apply)
        (cells.slice(0, 3), cells.slice(3, 6), cells.slice(6, 9))
      }
      def cellsForUnit(f: ((Seq[Cell], Seq[Cell], Seq[Cell])) => Seq[Cell]): Seq[Cell] =
        f(unitCells(0)) ++ f(unitCells(1)) ++ f(unitCells(2))
      Seq(Unit(cellsForUnit(_._1)), Unit(cellsForUnit(_._2)), Unit(cellsForUnit(_._3)))
    }.toSeq
    Puzzle(name, units)
  }.toSeq

  println(puzzles(0))
}
