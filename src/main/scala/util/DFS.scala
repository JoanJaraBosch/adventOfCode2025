package util

import scala.annotation.tailrec

case object DFS {

  case class PathState[T](value: T, steps: Int)

  def solve[T](start: T)(goal: T => Boolean)(next: (T, Int) => Seq[T]): Option[PathState[T]] = {
    def helper(current: PathState[T], seen: Set[T]): (Option[PathState[T]], Set[T]) = {
      val nextStates = next(current.value, current.steps).filterNot(seen.contains)

      nextStates.find(goal) match {
        case Some(state) => (Some(PathState(state, current.steps + 1)), seen)
        case None =>
          @tailrec
          def innerLoop(remainingStates: Seq[T], seen: Set[T]): (Option[PathState[T]], Set[T]) = {
            if (remainingStates.isEmpty) (None, seen)
            else {
              val (next, rest) = (remainingStates.head, remainingStates.tail)
              val (nextResult, nextSeen) = helper(PathState(next, current.steps + 1), seen)

              if (nextResult.isDefined) (nextResult, nextSeen)
              else innerLoop(rest, seen + next)
            }
          }

          innerLoop(nextStates, seen ++ nextStates)
      }
    }

    if (goal(start)) Some(PathState(start, 0))
    else helper(PathState(start, 0), Set(start))._1
  }

  def reachable[T](start: T)(next: (T, Int) => Seq[T]): Set[T] = {
    def helper(current: PathState[T], seen: Set[T]): Set[T] = {
      val nextStates = next(current.value, current.steps)

      nextStates
        .filterNot(seen.contains)
        .foldLeft(seen ++ nextStates) { (seen, state) =>
          seen ++ helper(PathState(state, current.steps + 1), seen)
        }
    }

    helper(PathState(start, 0), Set(start))
  }

}
