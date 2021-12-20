package org.clulab.wm.eidoscommon.utils

import org.clulab.wm.eidoscommon.utils.IdentityHashMap.IdentityHashMap

class IdentityHashBag[K <: AnyRef](map: IdentityHashMap[K, Int]) extends HashBag[K](map) {

  def this() = this(IdentityHashMap[K, Int]())
}

object IdentityHashBag {

  def apply[T <: AnyRef](): IdentityHashBag[T] = new IdentityHashBag[T]()

  def apply[T <: AnyRef](values: Seq[T]): IdentityHashBag[T] = apply[T]() ++ values

  def apply[T <: AnyRef](values: Seq[T], getNeighbors: T => Iterable[T]): IdentityHashBag[T] =
      apply[T]() ++ (values, getNeighbors)
}
