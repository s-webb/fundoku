package fundoku

import org.scalatest.{Matchers, WordSpecLike}

class PuzzleSpec extends WordSpecLike with Matchers {

  "charToCell" should {
    "build empty cell" in {
      Puzzle.charToCell('0') shouldBe Some(Puzzle.emptyCell)
    }

    "build completed cell" in {
      Puzzle.charToCell('3') shouldBe Some(Puzzle.completedCell(3))
    }

    "return none if input is invalid" in {
      Puzzle.charToCell('a') shouldBe None
    }
  }
  
  "Parsers.name" should {
    "return name and remaining chars from input" in {
      Puzzle.Parsers.name("name\nrest".toStream) shouldBe Some(("name", "rest".toStream))
    }

    "return None if no newlines in input" in {
      Puzzle.Parsers.name("name".toStream) shouldBe None
    }

    "return None if empty input" in {
      Puzzle.Parsers.name("".toStream) shouldBe None
    }

    "return None if immediate newline in input" in {
      Puzzle.Parsers.name("\nrest".toStream) shouldBe None
    }
  }

  "Parsers.grid" should {
    "populate cell array" in {
      val parsed = Puzzle.Parsers.grid(gridText.toStream)
      parsed shouldBe defined
      val (l, r) = parsed.get
      l should equal (gridCells)
      r shouldBe empty
    }
  }

  "Parsers.puzzle" should {
    "parse name and grid" in {
      val in = "name\n" + gridText
      val ((name, grid), rem) = Puzzle.Parsers.puzzle(in.toStream).get
      name shouldBe "name"
      grid should equal (gridCells)
      rem shouldBe empty
    }
  }

  "Parsers.puzzle2" should {
    "parse name and grid" in {
      val in = "name\n" + gridText
      val ((name, grid), rem) = Puzzle.Parsers.puzzle2(in.toStream).get
      name shouldBe "name"
      grid should equal (gridCells)
      rem shouldBe empty
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

  val gridCells: Array[Puzzle.Cell] = Array(
    1,2,3,1,2,3,1,2,3,
    2,2,3,1,2,3,1,2,3,
    3,2,3,1,2,3,1,2,3,
    4,2,3,1,2,3,1,2,3,
    5,2,3,1,2,3,1,2,3,
    6,2,3,1,2,3,1,2,3,
    7,2,3,1,2,3,1,2,3,
    8,2,3,1,2,3,1,2,3,
    9,2,3,1,2,3,1,2,3).map(Puzzle.completedCell)
}
