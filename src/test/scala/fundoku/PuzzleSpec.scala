package fundoku

import org.scalatest.{Matchers, WordSpecLike}

class PuzzleSpec extends WordSpecLike with Matchers {

  import Puzzle._

  "charToCell" should {
    "build empty cell" in {
      charToCell(0, '0') shouldBe Some(emptyCell(0))
    }

    "build completed cell" in {
      charToCell(0, '3') shouldBe Some(completedCell(0, 3))
    }

    "return none if input is invalid" in {
      charToCell(0, 'a') shouldBe None
    }
  }
  
  "Parsers.name" should {
    "return name and remaining chars from input" in {
      Parsers.name("name\nrest".toStream) shouldBe Some(("name", "rest".toStream))
    }

    "return None if no newlines in input" in {
      Parsers.name("name".toStream) shouldBe None
    }

    "return None if empty input" in {
      Parsers.name("".toStream) shouldBe None
    }

    "return None if immediate newline in input" in {
      Parsers.name("\nrest".toStream) shouldBe None
    }
  }

  "Parsers.grid" should {
    "populate cell array" in {
      val parsed = Parsers.grid(gridText.toStream)
      parsed shouldBe defined
      val (l, r) = parsed.get
      l should equal (gridCells)
      r shouldBe empty
    }
  }

  "Parsers.puzzle" should {
    "parse name and grid" in {
      val in = "name\n" + gridText
      val ((name, grid), rem) = Parsers.puzzle(in.toStream).get
      name shouldBe "name"
      grid should equal (gridCells)
      rem shouldBe empty
    }
  }

  "Parsers.puzzle2" should {
    "parse name and grid" in {
      val in = "name\n" + gridText
      val ((name, grid), rem) = Parsers.puzzle2(in.toStream).get
      name shouldBe "name"
      grid should equal (gridCells)
      rem shouldBe empty
    }
  }

  "PuzzleExtensions" should {
    "return first row" in {
      emptyPuzzle.row(0) should be ((0 to 8).map(emptyCell).toArray)
    }
    "return last row" in {
      emptyPuzzle.row(8) should be ((72 to 80).map(emptyCell).toArray)
    }
    "return first column" in {
      emptyPuzzle.column(0) should be ((0 to 8).map(n => emptyCell(n * 9)).toArray)
    }
    "return last column" in {
      emptyPuzzle.column(8) should be ((0 to 8).map(n => emptyCell((n * 9) + 8)).toArray)
    }
    "return first unit" in {
      emptyPuzzle.unit(0) should be (Seq(0, 1, 2, 9, 10, 11, 18, 19, 20).map(emptyCell).toArray)
    }
    "return last unit" in {
      emptyPuzzle.unit(8) should be (Seq(60, 61, 62, 69, 70, 71, 78, 79, 80).map(emptyCell).toArray)
    }
  }

  "Solver.removeCompleted" should {
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
  }

  val gridText = """
    |123123123
    |223123123
    |323123123
    |423123123
    |523123123
    |623123123
    |723123123
    |823123123
    |923123123
    """.stripMargin.trim

  val gridCells: Array[Cell] = Array(
    1,2,3,1,2,3,1,2,3,
    2,2,3,1,2,3,1,2,3,
    3,2,3,1,2,3,1,2,3,
    4,2,3,1,2,3,1,2,3,
    5,2,3,1,2,3,1,2,3,
    6,2,3,1,2,3,1,2,3,
    7,2,3,1,2,3,1,2,3,
    8,2,3,1,2,3,1,2,3,
    9,2,3,1,2,3,1,2,3).zipWithIndex.map { case (n, i) =>
      completedCell(i, n)
    }

  val emptyPuzzle: Puzzle = {
    val cells = (0 to 81).map(emptyCell).toArray
    ("empty", cells)
  }
}
