package fundoku

import fundoku.ActivityPuzzle.{CellActivity, Cells}
import org.scalatest.{Matchers, WordSpecLike}

class ActivityPuzzleSpec extends WordSpecLike with Matchers {

  val puzzleText = """003020600
    |900305001
    |001806400
    |008102900
    |700000008
    |006708200
    |002609500
    |800203009
    |005010300""".stripMargin

  val noneEliminated = Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
  val puzzleCells = puzzleText.toSeq.flatMap { s =>
    if (s == '\n') None else {
      val c = if (s == '0') noneEliminated else Set(s.toString.toInt)
      Some(c)
    }
  }

  "listCompleted" should {
//    "track activity" in {
//      import ActivityPuzzle._
//      val cells = Seq[Set[Int]] (
//        Set(1),
//        noneEliminated,
//        noneEliminated,
//        noneEliminated,
//        noneEliminated,
//        noneEliminated,
//        noneEliminated,
//        noneEliminated,
//        noneEliminated
//      )
//      val p: (Cells, CellActivity) = (cells, initialActivity.runEmptyA.value)
//      ActivityPuzzle.activity(0, 0).run(p).value._2 should be (0)
//
//      val row = 0
//      val completedCells = (0 to 8).flatMap { col =>
//        if (isCompleted(row, col).run(p).value._2) Some(col) else None
//      }
//      val completedDigits = completedCells.map { col =>
//        p.answer(row, col)
//      }
//      completedDigits should have size 1
//      p.activity(row, 0) should be (2)
//      (1 to 8).foreach { col =>
//        p.activity(row, col) should be (1)
//      }
//
//      for {
//        col <- 0 to 8
//        digit <- completedDigits
//      } {
//        if (!(completedCells contains col)) {
//          p.removeCandidate(row, col, digit)
//        }
//      }
//
//      (0 to 8).foreach { col =>
//        println(s"($row,$col): ${p.printCandidates(row, col)} (${p.activity(row, col)})")
//      }
//    }
  }

  "solve" should {
    import ActivityPuzzle._
    "solve a puzzle" in {
      val p: (Cells, CellActivity) = (puzzleCells, initialActivity.runA(puzzleCells).value)
      var eliminated = true
      var step = 0
      println(s"Step: $step (activity: ${totalActivity.runA(p).value})")
      println(p + "\n")

      while (!isSolved.runA(puzzleCells).value && eliminated) {
        step += 1
        eliminated = eliminateOnce(p)
        println(s"Step: $step (activity: ${totalActivity.runA(p).value})")
        println(p + "\n")
      }
      println(s"solved? ${isSolved.runA(puzzleCells).value}")
    }
  }

}
