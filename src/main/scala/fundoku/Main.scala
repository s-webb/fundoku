package fundoku

import scala.collection.JavaConverters._
import scala.collection.mutable.Buffer

import cats._
import cats.syntax.cartesian._

object Puzzle {

  type Cell = (Int, Set[Int])

  implicit class CellExtensions(cell: Cell) {
    def print: String =
      if (isCompleted) cell._2.head.toString else " "

    def numCandidates: Int = cell._2.size
    def isCompleted: Boolean = numCandidates == 1
    def eliminate(digits: Set[Int]): Option[Cell] = {
      if (!isCompleted && (cell._2 intersect digits).size > 0) {
        Some((cell._1, cell._2 -- digits))
      } else None
    }
  }

  type Puzzle = (String, Seq[Cell])

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
      (puzzle._1, updateCells(puzzle._2, changes))
    }

    def print: String =
      (0 to 9).map(n => row(n).map(_.print).mkString).mkString("\n")
  }

  def updateCells(cells: Seq[Cell], changes: Seq[Cell]) = 
    changes.foldRight(cells) { case (change, cs) =>
      cs.updated(change._1, change)
    }

  def puzzle(name: String, cells: Seq[Cell]): Puzzle = {
    (name, cells)
  }

  def emptyCell(i: Int): Cell = (i, (1 to 9).toSet)
  def completedCell(i: Int, n: Int): Cell = (i, Set(n))

  def charToCell(i: Int, ch: Char): Option[Cell] = ch match {
      case '0' => Some(emptyCell(i))
      case n if n.isDigit => Some(completedCell(i, n.toString.toInt))
      case _ => None
    }

  type Parser[A] = Stream[Char] => Option[(A, Stream[Char])]

  object Parsers {

    val grid: Parser[Seq[Cell]] = in => {
      val (l, r) = in.splitAt(90)
      val cells = l.foldLeft(Buffer[Cell]()) {(cs, c) =>
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

}

