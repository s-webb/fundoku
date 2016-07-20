package fundoku

import cats.data.State

object Solver {

  import Puzzle._

  val eliminateRows = State(eliminateGroups(_.row))
  val eliminateCols = State(eliminateGroups(_.column))
  val eliminateUnits = State(eliminateGroups(_.unit))

  val eliminateAll: State[Puzzle, Boolean] = for {
    updatedRows <- eliminateRows
    updatedCols <- eliminateCols
    updatedUnits <- eliminateUnits
  } yield {
    updatedRows || updatedCols || updatedUnits
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

  def eliminateGroups(f: (Puzzle) => Int => Seq[Cell], range: Range = (0 to 8))
      (puzzle: Puzzle): (Puzzle, Boolean) = {

    range.foldLeft((puzzle, false)) { case (in @ (p, _), i) =>
      eliminateSingleGroup(i, p, f).map((_, true)).getOrElse(in)
    }
  }

  def eliminateSingleGroup(i: Int, p: Puzzle, f: (Puzzle) => Int => Seq[Cell]): Option[Puzzle] = {
    val changes = eliminateCompleted(f(p)(i))
    if (changes.isEmpty) None else Some(p.update(changes))
  }

  def eliminateCompleted(cells: Seq[Cell]): Seq[Cell] = {
    val completed: Set[Int] = cells.filter(_.isCompleted).foldRight(Set[Int]())(_._2 ++ _)
    cells.flatMap(_.eliminate(completed))
  }

  // in order to log activity, I'd need to abstract over all of the operations against cells
  // what cell operations are there?
  //
  //   - check if completed (check size of a set, how is this implemented?)
  //   - find intersection of candidates and another set of digits
  //   - remove set of digits from candidates
  //
  // Can I define an API for working with cells then?

  // What's that other one?
  //  - find any pair of cells in a row/col/unit that have the same remaining digits
  //  - eliminate those digits from all other incomplete cells in the same row/col/unit
  //  - also works for higher arity (e.g. three cells with three candidates between then, or four ...)
  //
  // Or constraint propagation (Norvig)

}
