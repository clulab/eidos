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
}
