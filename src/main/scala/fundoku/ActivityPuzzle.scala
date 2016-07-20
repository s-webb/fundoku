package fundoku

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
class ActivityPuzzle(var cells: Seq[Set[Int]]) {

  var _activity: Seq[Int] = Seq.fill(cells.size)(0)

  def totalActivity: Int = _activity.sum

  def activity(r: Int, c: Int): Int =
    _activity(rowAndColToIndex(r, c))

  def isCompleted(r: Int, c: Int): Boolean = {
    incActivity(r, c)
    candidatesAt(r, c).size == 1
  }

  def isSolved: Boolean = {
    cells.forall(_.size == 1)
  }

  def answer(r: Int, c: Int): Int = {
    incActivity(r, c)
    candidatesAt(r, c).iterator.next
  }

  def printCandidates(r: Int, c: Int): String = {
    cells(rowAndColToIndex(r, c)).toSeq.sorted.mkString(",")
  }

  /**
   * Cost of removing a candidate is proportional to number of candidates in the cell
   */
  def removeCandidate(r: Int, c: Int, candidate: Int): Boolean = {
    val index = rowAndColToIndex(r, c)
    val currCandidates = cells(index)
    var newCandidates = Set[Int]()
    val it = currCandidates.iterator
    var eliminated = false
    while (it.hasNext) {
      incActivity(r, c)
      val v = it.next
      if (v != candidate) {
        newCandidates = newCandidates + v
      } else {
        eliminated = true
      }
    }
    cells = cells.updated(index, newCandidates)
    eliminated
  }

  private def incActivity(r: Int, c: Int): Unit = {
    val index = rowAndColToIndex(r, c)
    val curr = _activity(index)
    _activity = _activity.updated(index, curr + 1)
  }

  private def candidatesAt(r: Int, c: Int): Set[Int] =
    cells(rowAndColToIndex(r, c))

  private def rowAndColToIndex(r: Int, c: Int): Int = (r * 9) + c

  override def toString: String = {
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
