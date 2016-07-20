package fundoku

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
    "track activity" in {
      val cells = Seq[Set[Int]] (
        Set(1),
        noneEliminated,
        noneEliminated,
        noneEliminated,
        noneEliminated,
        noneEliminated,
        noneEliminated,
        noneEliminated,
        noneEliminated
      )
      val p = new ActivityPuzzle(cells)
      p.activity(0, 0) should be (0)

      val row = 0
      val completedCells = (0 to 8).flatMap { col =>
        if (p.isCompleted(row, col)) Some(col) else None
      }
      val completedDigits = completedCells.map { col =>
        p.answer(row, col)
      }
      completedDigits should have size 1
      p.activity(row, 0) should be (2)
      (1 to 8).foreach { col =>
        p.activity(row, col) should be (1)
      }

      for {
        col <- 0 to 8
        digit <- completedDigits
      } {
        if (!(completedCells contains col)) {
          p.removeCandidate(row, col, digit)
        }
      }

      (0 to 8).foreach { col =>
        println(s"($row,$col): ${p.printCandidates(row, col)} (${p.activity(row, col)})")
      }
    }
  }

  "solve" should {
    "solve a puzzle" in {
      val p = new ActivityPuzzle(puzzleCells)
      var eliminated = true
      var step = 0
      println(s"Step: $step (activity: ${p.totalActivity})")
      println(p + "\n")

      while (!p.isSolved && eliminated) {
        step += 1
        eliminated = eliminateOnce(p)
        println(s"Step: $step (activity: ${p.totalActivity})")
        println(p + "\n")
      }
      println(s"solved? ${p.isSolved}")
    }
  }

  /**
   * To animate this, you'd want eliminate once to give you back the new puzzle state and a per-cell activity value
   */
  def eliminateOnce(p: ActivityPuzzle): Boolean = {
    val groups: Seq[Int => Seq[(Int, Int)]] = Seq(rowIndices, colIndices, unitIndices)
    val eliminated = 
      for {
        g <- groups
        n <- 0 to 8
      } yield {
        eliminateCells(p, g(n))
      }
    eliminated.fold(false)(_ || _)
  }

  def rowIndices(r: Int): Seq[(Int, Int)] = (0 to 8).map((r, _))
  def colIndices(c: Int): Seq[(Int, Int)] = (0 to 8).map((_, c))

  def unitIndices(u: Int): Seq[(Int, Int)] = {
    val firstRow = (u / 3) * 3
    val lastRow = firstRow + 3
    val firstCol = (u % 3) * 3
    val lastCol = firstCol + 3
    for {
      r <- firstRow until lastRow
      c <- firstCol until lastCol
    } yield (r, c)
  }

  def eliminateCells(p: ActivityPuzzle, indices: Seq[(Int, Int)]): Boolean = {
    val (complete, incomplete) = indices.partition(i => p.isCompleted(i._1, i._2))
    val completedDigits = complete.map(i => p.answer(i._1, i._2))
    val es = for {
      index <- incomplete
      digit <- completedDigits
    } yield {
      p.removeCandidate(index._1, index._2, digit)
    }
    es.fold(false)(_ || _)
  }
}
