package fundoku

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.collection.JavaConverters._
import scala.collection.mutable.Buffer

import cats._
import cats.syntax.cartesian._

object Puzzle {

  type Cell = (Int, Set[Int])
  type Puzzle = (String, Seq[Cell])

  def puzzle(name: String, cells: Seq[Cell]): Puzzle = {
    (name, cells)
  }

  def emptyCell(i: Int): Cell = (i, (1 to 9).toSet)
  def completedCell(i: Int, n: Int): Cell = (i, Set(n))
  def numCandidates(c: Cell): Int = c._2.size
  def isCompleted(c: Cell): Boolean = numCandidates(c) == 1
  def eliminate(c: Cell, digits: Set[Int]): Option[Cell] = {
    if (!isCompleted(c) && (c._2 intersect digits).size > 0) {
      Some((c._1, c._2 -- digits))
    } else None
  }

  def charToCell(i: Int, ch: Char): Option[Cell] = ch match {
      case '0' => Some(emptyCell(i))
      case n if n.isDigit => Some(completedCell(i, n.toString.toInt))
      case _ => None
    }

  type Parser[A] = Stream[Char] => Option[(A, Stream[Char])]

  object Parsers {

    val grid: Parser[Seq[Cell]] = in => {
      val (l, r) = in.splitAt(90)
      val cells = l.foldRight(Buffer[Cell]()) {(c, cs) =>
        charToCell(cs.size, c).map(cs += _).getOrElse(cs)
      }
      // val cells = l.flatMap(charToCell)
      if (cells.size == 81) Some(cells.toSeq, r) else None
    }

    val name: Parser[String] = in => {
      def step(cs: Stream[Char], acc: Option[String]): Option[(String, Stream[Char])] =
        cs.headOption.flatMap { c =>
          if (c == '\n') {
            acc.map((_, cs.tail))
          } else {
            val stepAcc = acc.map(_ + c.toString) orElse Some(c.toString)
            step(cs.tail, stepAcc)
          }
        }

      step(in, None)
    }

    // implementation of map2 for parsers
    def parse2[A, B, C](pa: Parser[A], pb: Parser[B], f: (A, B) => C): Parser[C] = s => {
      for {
        (a, s1) <- pa(s)
        (b, s2) <- pb(s1)
      } yield (f(a, b), s2)
    }

    val puzzle: Parser[Puzzle] = 
      parse2(name, grid, Puzzle.puzzle)

    // ... but I could get map2 from cats.Apply if I can implement ap and map for Parser[_]
    implicit val parserApply: Apply[Parser] = new Apply[Parser] {
      def ap[A, B](pf: Parser[A => B])(pa: Parser[A]): Parser[B] = s => {
        for {
          (f, s1) <- pf(s)
          (a, s2) <- pa(s1)
        } yield (f(a), s2)
      }

      def map[A, B](pa: Parser[A])(f: A => B): Parser[B] = s => {
        pa(s).map { case (a, s1) =>
          (f(a), s1)
        }
      }
    }

    val puzzle2: Parser[Puzzle] =
      (name |@| grid) map { Puzzle.puzzle }
      // Apply[Parser].map2(name, grid)(Puzzle.puzzle)

    // I guess it could be handy to compose stuff like this if we had more parsers
  }

  // Implement toString for Puzzle...

  implicit class PuzzleExtensions(puzzle: Puzzle) {
    def row(idx: Int): Seq[Cell] = {
      puzzle._2.slice(idx * 9, (idx * 9) + 9)
    }

    def column(idx: Int): Seq[Cell] = {
      ((0 until 9) map (_ * 9 + idx) map puzzle._2.apply).toSeq
    }

    def unit(idx: Int): Seq[Cell] = {
      val r = (idx / 3) * 3
      val c = (idx % 3) * 3
      (r until (r + 3)).flatMap(row(_).slice(c, c + 3)).toSeq
    }

    def update(changes: Seq[Cell]): Puzzle = {
      val newCells = changes.foldRight(puzzle._2) { case (change, cells) =>
        cells.updated(change._1, change)
      }
      (puzzle._1, newCells)
    }
  }
}

object Solver {

  import Puzzle._

  def removeCompleted(cells: Seq[Cell]): Seq[Cell] = {
    val completed: Set[Int] = cells.filter(isCompleted).foldRight(Set[Int]())(_._2 ++ _)
    cells.flatMap(eliminate(_, completed))
  }

  // What would a solver look like?
  //
  // This strategy is too simple to solve many puzzles
  // - take each row/col/unit in turn
  // - remove any digits that are already completed
  // - repeat until the puzzle is solved
  // - ...
  // - profit
  def solveBySimpleElimination(puzzle: Puzzle): Option[Puzzle] = {
    def step(i: Int, p: Puzzle, f: (Int, Puzzle) => Seq[Cell]): Option[Puzzle] = {
      val changes = removeCompleted(f(i, p))
      if (changes.size == 0) None else Some(p.update(changes))
    }

    // eliminate over rows
    val (afterRows, updatedRows) = (0 to 8).foldRight((puzzle, false)) { case (i, in@(p, updated)) => 
      step(i, p, (i, p) => p.row(i)).map((_, true)).getOrElse(in)
    }

    // eliminate over cols
    val (afterCols, updatedCols) = (0 to 8).foldRight((afterRows, false)) { case (i, in@(p, updated)) => 
      step(i, p, (i, p) => p.column(i)).map((_, true)).getOrElse(in)
    }

    // eliminate over units
    val (afterUnits, updatedUnits) = (0 to 8).foldRight((afterCols, false)) { case (i, in@(p, updated)) => 
      step(i, p, (i, p) => p.unit(i)).map((_, true)).getOrElse(in)
    }

    if (afterUnits._2.forall(isCompleted(_))) {
      // if all cells are completed, we're done
      Some(afterUnits)
    } else if (updatedRows || updatedCols || updatedUnits) {
      // if any of them did anything, repeat
      solveBySimpleElimination(afterUnits)
    } else {
      // otherwise, don't, return None (couldn't solve)
      None
    }
  }

  // What's that other one?
  //  - find any pair of cells in a row/col/unit that have the same remaining digits
  //  - eliminate those digits from all other incomplete cells in the same row/col/unit
  //  - also works for higher arity (e.g. three cells with three candidates between then, or four ...)
  //
  // Or constraint propagation (Norvig)
}
