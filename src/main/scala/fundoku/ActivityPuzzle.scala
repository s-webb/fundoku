package fundoku

import cats.data.State
import fundoku.ActivityPuzzle.{CellActivity, CellState, Cells}

import scala.annotation.tailrec

/**
  * An implementation of puzzle that tracks activity
  *
  * Have had to make some fairly arbitrary decisions when calculating
  * the cost of each operation. Same rules apply to any code written against
  * the API though.
  *
  * Would be interesting to be able to be able to have cost rules for
  * different underlying datastructures.
  */
object ActivityPuzzle {

  type Cells = Seq[Set[Int]]

  type CellActivity = Seq[Int]

  type CellState[A] = State[Cells, A]

  type ActivityPuzzleState[A] = State[(Cells, CellActivity), A]

  val initialActivity: CellState[CellActivity]= State.inspect(cells => Seq.fill(cells.size)(0))

  def totalActivity: ActivityPuzzleState[Int] = State.inspect{ case (cells, activity) => activity.sum }

  def activity(r: Int, c: Int): ActivityPuzzleState[Int] = State.inspect {
    case (cells, activity) => activity(rowAndColToIndex(r, c))
  }

  def isCompleted(r: Int, c: Int): ActivityPuzzleState[Boolean] = {
    for {
      _ <- incActivity(r, c)
      cs <- candidatesAt(r, c)
    } yield {
      cs.size == 1
    }
  }

  val isSolved: CellState[Boolean] = State.inspect(_.forall(_.size == 1))

  def answer(r: Int, c: Int): ActivityPuzzleState[Int] = {
    for {
      _ <- incActivity(r, c)
      cs <- candidatesAt(r, c)
    } yield {
      cs.head
    }
  }

  def printCandidates(r: Int, c: Int): CellState[String] = State.inspect { cells =>
    cells(rowAndColToIndex(r, c)).toSeq.sorted.mkString(",")
  }

  /**
   * Cost of removing a candidate is proportional to number of candidates in the cell
   */
  def removeCandidate(r: Int, c: Int, candidate: Int): ActivityPuzzleState[Boolean] = State { case (cells, activity) =>
    val index = rowAndColToIndex(r, c)
    val currCandidates = cells(index)

    // Recursion with states???
    @tailrec
    def eliminate(candidates: Set[Int], newCandidates: Set[Int], eliminated: Boolean): (Set[Int], Boolean) = {
      if (candidates.isEmpty) {
        (newCandidates, eliminated)
      } else {
        val v = candidates.head
        incActivity(r, c) // TODO - how to get recursion to play nicely with a state monad?
        if (v != candidate) {
          eliminate(candidates.tail, newCandidates + v, eliminated)
        } else {
          eliminate(candidates.tail, newCandidates, true)
        }
      }
    }

    val (newCandidates, eliminated) = eliminate(currCandidates, Set.empty, false)
    (cells.updated(index, newCandidates) -> activity, eliminated)
  }

  private def incActivity(r: Int, c: Int): ActivityPuzzleState[Unit] = State { case (cells, activity) =>
    val index = rowAndColToIndex(r, c)
    val curr = activity(index)
    (cells -> activity.updated(index, curr + 1), ())
  }

  private def candidatesAt(r: Int, c: Int): ActivityPuzzleState[Set[Int]] = State.inspect {
    case (cells, activity) => cells(rowAndColToIndex(r, c))
  }

  private def rowAndColToIndex(r: Int, c: Int): Int = (r * 9) + c

  val asString: CellState[String] = State.inspect { cells =>
    val lines =
      (0 to 8).map { r =>
        val line = (0 to 8).map { c =>
          val index = rowAndColToIndex(r, c)
          if (cells(index).size == 1) {
            cells(index).iterator.next.toString
          } else {
            "_"
          }
        }
        val chunks = Seq(line.slice(0, 3), line.slice(3, 6), line.slice(6, 9))
        chunks.map(_.mkString("")).mkString(" ")
      }
    val chunks = Seq(lines.slice(0, 3), lines.slice(3, 6), lines.slice(6, 9))
    chunks.map(_.mkString("\n")).mkString("\n\n")
  }
}
