package org.clulab.wm.eidoscommon.utils

import org.clulab.wm.eidoscommon.utils.EqualityHashMap.EqualityHashMap

class EqualityHashBag[T](map: EqualityHashMap[T, Int]) extends HashBag[T](map) {

  def this() = this(new EqualityHashMap[T, Int]())
}

object EqualityHashBag {

  def apply[T](): EqualityHashBag[T] = new EqualityHashBag[T]()

  def apply[T](values: Seq[T]): EqualityHashBag[T] = apply[T]() ++ values

  def apply[T](values: Seq[T], getNeighbors: T => Iterable[T]): EqualityHashBag[T] =
      apply[T]() ++ (values, getNeighbors)
}
