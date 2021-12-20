package org.clulab.wm.eidoscommon.utils

import scala.collection.mutable

object EqualityHashSet {
  type EqualityHashSet[K] = mutable.HashSet[K]

  def apply[T](): EqualityHashSet[T] = new EqualityHashSet[T]()

  def apply[T](values: Seq[T]): EqualityHashSet[T] = apply[T]() ++ values
}
