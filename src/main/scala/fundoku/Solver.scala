package fundoku

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

    // eliminate over rows
    // val (afterRows, updatedRows) = (0 to 8).foldRight((puzzle, false)) { case (i, in@(p, updated)) => 
    //   eliminateGroup(i, p, (i, p) => p.row(i)).map((_, true)).getOrElse(in)
    // }
    val (afterRows, updatedRows) = eliminateGroups(0 to 8, puzzle, (n, p) => p.row(n))

    // eliminate over cols
    // val (afterCols, updatedCols) = (0 to 8).foldRight((afterRows, false)) { case (i, in@(p, updated)) => 
    //   eliminateGroup(i, p, (i, p) => p.column(i)).map((_, true)).getOrElse(in)
    // }
    val (afterCols, updatedCols) = eliminateGroups(0 to 8, afterRows, (n, p) => p.column(n))

    // eliminate over units
    // val (afterUnits, updatedUnits) = (0 to 8).foldRight((afterCols, false)) { case (i, in@(p, updated)) => 
    //   eliminateGroup(i, p, (i, p) => p.unit(i)).map((_, true)).getOrElse(in)
    // }
    val (afterUnits, updatedUnits) = eliminateGroups(0 to 8, afterCols, (n, p) => p.unit(n))

    if (afterUnits._2.forall(_.isCompleted)) {
      // if all cells are completed, we're done
      Some(afterUnits)
    } else if (updatedRows || updatedCols || updatedUnits) {
      // if any of them did anything, repeat
      solveBySimpleElimination(afterUnits)
    } else {
      // otherwise, don't, return None (couldn't solve)
      None
    }
  }

  def eliminateGroups(range: Range, puzzle: Puzzle, f: (Int, Puzzle) => Seq[Cell]): (Puzzle, Boolean) = {
    range.foldLeft((puzzle, false)) { case (in@(p, _), i) => 
      eliminateGroup(i, p, f).map((_, true)).getOrElse(in)
    }
  }

  def eliminateGroup(i: Int, p: Puzzle, f: (Int, Puzzle) => Seq[Cell]): Option[Puzzle] = {
    val changes = removeCompleted(f(i, p))
    if (changes.size == 0) None else Some(p.update(changes))
  }

  def removeCompleted(cells: Seq[Cell]): Seq[Cell] = {
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
