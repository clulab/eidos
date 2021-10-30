package org.clulab.wm.eidoscommon.utils

import scala.collection.mutable

class HashBag[K](protected val map: mutable.Map[K, Int]) extends mutable.Set[K] {

  override def +=(elem: K): HashBag.this.type = {
    val count = map.getOrElse(elem, 0) + 1

    map(elem) = count
    this
  }

  override def -=(elem: K): HashBag.this.type = {
    val count = map.getOrElse(elem, 0) - 1

    if (count > 0)
      map(elem) = count
    else
      map.remove(elem)
    this
  }

  override def contains(elem: K): Boolean = map.contains(elem)

  override def iterator: Iterator[K] = map.keysIterator

  def count(elem: K): Int = map(elem)
}
