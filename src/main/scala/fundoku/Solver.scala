package fundoku

import scala.language.higherKinds

import cats._
import cats.data.State
import cats.std.list._
import cats.syntax.traverse._

object Solver {

  import Puzzle._

  // What would a solver look like?
  //
  // This strategy is too simple to solve many puzzles
  // - take each row/col/unit in turn
  // - remove any digits that are already completed
  // - repeat until the puzzle is solved
  // - ...
  // - profit
  def solveBySimpleElimination(puzzle: Puzzle): Option[Puzzle] = {
    val (updatedPuzzle, modified) = eliminateAll.run(puzzle).value
    if (updatedPuzzle.solved) {
      // if all cells are completed, we're done
      Some(updatedPuzzle)
    } else if (modified) {
      // if any of them did anything, repeat
      solveBySimpleElimination(updatedPuzzle)
    } else {
      // otherwise, don't, return None (couldn't solve)
      None
    }
  }

  def solveBySimpleElimination1: State[Puzzle, Boolean] = {
    // not very satisfactory
    eliminateAll.transform { (p, modified) =>
      if (p.solved) {
        (p, true) 
      } else if (modified) {
        solveBySimpleElimination1.run(p).value
      } else {
        (p, false)
      }
    }
  }

  type GroupForIndex = Int => Seq[Cell]
  type GroupSelector = Puzzle => GroupForIndex

  val orMonoid = new Monoid[Boolean] {
    def empty: Boolean = false
    def combine(b1: Boolean, b2: Boolean): Boolean = b1 || b2
  }

  def foldPuzzleStates[A: Monoid, F[_]: Traverse](states: F[State[Puzzle, A]]): State[Puzzle, A] = {
    states.sequenceU.map(Foldable[F].fold(_))
  }

  val groups = List[GroupSelector](_.row, _.column, _.unit)

  val eliminateAll: State[Puzzle, Boolean] = {
    implicit val or = orMonoid
    foldPuzzleStates(groups.map(eliminateGroups(_)))
  }

  def eliminateGroups(f: GroupSelector, range: Range = (0 to 8)): State[Puzzle, Boolean] = {
    implicit val or = orMonoid
    val single = eliminateSingleGroup(f) _
    foldPuzzleStates(range.toList.map(single))
  }

  def eliminateSingleGroup(f: GroupSelector)(i: Int): State[Puzzle, Boolean] = {
    State { p => 
      val changes = eliminateCompleted(f(p)(i))
      (p.update(changes), !changes.isEmpty)
    }
  }

  def eliminateCompleted(cells: Seq[Cell]): Seq[Cell] = {
    val completed: Set[Int] = cells.filter(_.isCompleted).foldRight(Set[Int]())(_._2 ++ _)
    cells.flatMap(_.eliminate(completed))
  }

  // What's that other one?
  //  - find any pair of cells in a row/col/unit that have the same remaining digits
  //  - eliminate those digits from all other incomplete cells in the same row/col/unit
  //  - also works for higher arity (e.g. three cells with three candidates between then, or four ...)
  //
  // Or constraint propagation (Norvig)

}
