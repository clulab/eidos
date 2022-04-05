package org.clulab.wm.eidos.groundings.grounders.srl

import org.clulab.wm.eidoscommon.utils.Collection

abstract class ProximityDetector() {
  def detect(index: Int, intArgs: Seq[Int], intPreds: Seq[Int]): Int

  def indexAndCountToCount(position: Int, leftIndexAndCountOpt: Option[(Int, Int)], rightIndexAndCountOpt: Option[(Int, Int)]): Int = {
    (leftIndexAndCountOpt, rightIndexAndCountOpt) match {
      case (None, None) => 0
      case (None, Some((_, rightCount))) => rightCount
      case (Some((_, leftCount)), None) => leftCount
      case (Some((leftIndex, leftCount)), Some((rightIndex, rightCount))) =>
        // Whichever is closest to the position and when tied, the value that
        // is higher than position because we favor right, even though it might
        // be the higher numbered arg or pred.
        val leftDist = math.abs(position - leftIndex)
        val rightDist = math.abs(position - rightIndex)
        if (leftDist < rightDist) leftCount
        else if (leftDist == rightDist) {
          // These should not ever be equal because then there would be an
          // arg and pred at the same position.
          if (leftIndex > rightIndex) leftIndex
          else rightIndex
        }
        else rightCount
    }
  }
}

class RightLeftProximityDetector() extends ProximityDetector() {

  def detect(index: Int, intArgs: Seq[Int], intPreds: Seq[Int]): Int = {
    // Prefer looking to the right, after.
    val argWhereAndWhatOpt: Option[(Int, Int)] = Collection.findWhereAndWhatOptAfter(intArgs, index)(_ != 0)
    val predWhereAndWhatOpt: Option[(Int, Int)] = Collection.findWhereAndWhatOptAfter(intPreds, index)(_ != 0)
    if (argWhereAndWhatOpt.isDefined || predWhereAndWhatOpt.isDefined)
      indexAndCountToCount(index, argWhereAndWhatOpt, predWhereAndWhatOpt)
    else {
      // Then look left if necessary, before.
      val argWhereAndWhatOpt = Collection.findWhereAndWhatOptBefore(intArgs, index)(_ != 0)
      val predWhereAndWhatOpt = Collection.findWhereAndWhatOptBefore(intPreds, index)(_ != 0)
      if (argWhereAndWhatOpt.isDefined || predWhereAndWhatOpt.isDefined) {
        indexAndCountToCount(index, argWhereAndWhatOpt, predWhereAndWhatOpt)
      }
      else 0
    }
  }
}

class AroundProximityDetector extends ProximityDetector {

  def findWhereAndWhatOptAround[T](values: Seq[T], position: Int)(f: T => Boolean): Option[(Int, T)] = {
    val whereAndWhatOptBefore = Collection.findWhereAndWhatOptBefore(values, position)(f)
    val whereAndWhatOptAfter = Collection.findWhereAndWhatOptAfter(values, position)(f)

    (whereAndWhatOptBefore, whereAndWhatOptAfter) match {
      case (None, None) => None
      case (None, some) => some
      case (some, None) => some
      case (Some(before), Some(after)) =>
        // Prefer the one after in case of ties.
        if (math.abs(after._1 - position) <= math.abs(before._1 - position)) Some(after)
        else Some(before)
    }
  }

  def detect(index: Int, intArgs: Seq[Int], intPreds: Seq[Int]): Int = {
    val argWhereAndWhatOpt: Option[(Int, Int)] = findWhereAndWhatOptAround(intArgs, index)(_ != 0)
    val predWhereAndWhatOpt: Option[(Int, Int)] = findWhereAndWhatOptAround(intPreds, index)(_ != 0)

    if (argWhereAndWhatOpt.isDefined || predWhereAndWhatOpt.isDefined)
      indexAndCountToCount(index, argWhereAndWhatOpt, predWhereAndWhatOpt)
    else
      0
  }
}
