package fundoku

import org.scalatest.{Matchers, WordSpecLike}

class SolverSpec extends WordSpecLike with Matchers {

  import Puzzle._

  "removeCompleted" should {
    "remove completed digits from row" in {
      val row = (0 until 9).map(emptyCell).toArray
      row(0) = completedCell(0, 1)
      val result = Solver.removeCompleted(row)
      val expected = (1 until 9).map((_, (2 to 9).toSet))
      result should have size 8
      expected.foreach { e =>
        result should contain(e)
      }
    }

    "remove completed from first row of first example" in {
      val row = "003020600".zipWithIndex.flatMap(c => charToCell(c._2, c._1)).toArray
      val changes = Solver.removeCompleted(row)
      val result = updateCells(row, changes)
      // 3, 2 and 6 should have been removed from all cells except indices 2, 4 and 6
      (0 to 8).filterNot(Set(2, 4, 6).contains).foreach { n =>
        result(n)._2 should contain noneOf (2, 3, 6)
        result(n)._2 should contain allOf (1, 4, 5, 7, 8, 9)
      }
      result(2)._2 should contain only (3)
      result(4)._2 should contain only (2)
      result(6)._2 should contain only (6)
    }
  }

  val firstPuzzle = {
    val puzzleText: Stream[Char] = Puzzles.text.toStream
    val parsed = Parsers.puzzle2(Puzzles.firstPuzzle.toStream)
    parsed.isDefined should be (true)
    val p = parsed.get._1
    p._1 should be ("Grid 01")
    p
  }

  "solveBySimpleElimination" should {
    "solve something" in {
      val solution = Solver.solveBySimpleElimination(firstPuzzle)
      solution.isDefined should be (true)
      println(solution.get.print)
    }
  }

  "eliminateGroup" should {
    "eliminate first row of first puzzle" in {
      val eliminated = Solver.eliminateGroup(0, firstPuzzle, (n, p) => p.row(n))
      eliminated.isDefined should be (true)

      val result = eliminated.get.row(0)
      // 3, 2 and 6 should have been removed from all cells except indices 2, 4 and 6
      (0 to 8).filterNot(Set(2, 4, 6).contains).foreach { n =>
        result(n)._2 should contain noneOf (2, 3, 6)
        result(n)._2 should contain allOf (1, 4, 5, 7, 8, 9)
      }
      result(2)._2 should contain only (3)
      result(4)._2 should contain only (2)
      result(6)._2 should contain only (6)
    }
  }

  "eliminate groups" should {
    "eliminate first two rows of first puzzle" in {
      val (eliminated, updated) = Solver.eliminateGroups(0 to 1, firstPuzzle, (n, p) => p.row(n))
      updated should be (true)

      val row0 = eliminated.row(0)
      // 3, 2 and 6 should have been removed from all cells except indices 2, 4 and 6
      (0 to 8).filterNot(Set(2, 4, 6).contains).foreach { n =>
        row0(n)._2 should contain noneOf (2, 3, 6)
        row0(n)._2 should contain allOf (1, 4, 5, 7, 8, 9)
      }
      row0(2)._2 should contain only (3)
      row0(4)._2 should contain only (2)
      row0(6)._2 should contain only (6)

      val row1 = eliminated.row(1)
      // 1, 3, 5 and 9 should have been removed from all cells except indices 0, 3, 5 and 8
      (0 to 8).filterNot(Set(0, 3, 5, 8).contains).foreach { n =>
        row1(n)._2 should contain noneOf (1, 3, 5, 9)
        row1(n)._2 should contain allOf (2, 4, 6, 7, 8)
      }
      row1(0)._2 should contain only (9)
      row1(3)._2 should contain only (3)
      row1(5)._2 should contain only (5)
      row1(8)._2 should contain only (1)
    }
  }
}
