package fundoku

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.collection.JavaConverters._

import cats._
import cats.syntax.cartesian._

object Puzzle {

  type Cell = Set[Int]
  type Puzzle = (String, Array[Cell])

  def puzzle(name: String, cells: Array[Cell]): Puzzle = {
    (name, cells)
  }

  val emptyCell: Cell = (1 to 9).toSet
  def completedCell(n: Int): Cell = Set(n)

  def charToCell(ch: Char): Option[Cell] = ch match {
    case '0' => Some(emptyCell)
    case n if n.isDigit => Some(completedCell(n.toString.toInt))
    case _ => None
  }

  type Parser[A] = Stream[Char] => Option[(A, Stream[Char])]

  object Parsers {

    val grid: Parser[Array[Cell]] = in => {
      val (l, r) = in.splitAt(90)
      val cells = l.flatMap(charToCell)
      if (cells.size == 81) Some(cells.toArray, r) else None
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

    // I guess it could be handy to compose stuff like this if we had more parsers
  }

  // Implement toString for Puzzle...
}
