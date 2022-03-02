package org.clulab.wm.eidoscommon.utils

object Collection {

  // Scala 2.13+ has a minOption on the collection, but we'll do it as an argument.
  def minOption[A](values: Seq[A])(implicit cmp: Ordering[A]): Option[A] =
      if (values.isEmpty) None
      else Some(values.min)

  // Scala 2.13+ has a maxOption on the collection, but we'll do it as an argument.
  def maxOption[A](values: Seq[A])(implicit cmp: Ordering[A]): Option[A] =
      if (values.isEmpty) None
      else Some(values.max)

  def findWhereWhatOptAfter[T](values: IndexedSeq[T], position: Int)(f: T => Boolean): Option[(Int, T)] = {
    var index = position + 1

    while (index < values.length) {
      if (f(values(index)))
        return Some(index, values(index))
      index += 1
    }
    None
  }

  def findWhereWhatOptBefore[T](values: IndexedSeq[T], position: Int)(f: T => Boolean): Option[(Int, T)] = {
    var index = position - 1

    while (index >= 0) {
      if (f(values(index)))
        return Some(index, values(index))
      index -= 1
    }
    None
  }

  def optIndexOf[T](seq: IndexedSeq[T], value: T): Option[Int] = {
    val index = seq.indexOf(value)

    if (index < 0) None
    else Some(index)
  }
}
