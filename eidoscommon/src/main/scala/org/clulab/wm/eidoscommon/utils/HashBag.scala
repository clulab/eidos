package org.clulab.wm.eidoscommon.utils

import scala.collection.mutable

class HashBag[T](protected val map: mutable.Map[T, Int]) extends mutable.Set[T] {

  // Return the new count.
  def addAndCount(elem: T): Int = {
    val count = map.getOrElse(elem, 0) + 1

    map(elem) = count
    count
  }

  override def +=(elem: T): HashBag.this.type = {
    val count = map.getOrElse(elem, 0) + 1

    map(elem) = count
    this
  }

  override def -=(elem: T): HashBag.this.type = {
    val count = map.getOrElse(elem, 0) - 1

    if (count > 0)
      map(elem) = count
    else if (count == 0)
      map.remove(elem)
    this
  }

  override def contains(elem: T): Boolean = map.contains(elem)

  override def iterator: Iterator[T] = map.keysIterator

  def count(elem: T): Int = map.getOrElse(elem, 0)

  def ++(elems: Seq[T]): HashBag.this.type = {
    elems.foreach { elem => this + elem }
    this
  }

  def ++(elems: Seq[T], getNeighbors: T => Iterable[T]): HashBag.this.type = {

    def loop(value: T): Unit = {
      // If the count is now 1, it is new and neighbors should be collected.
      if (addAndCount(value) == 1)
        getNeighbors(value).foreach(loop)
    }

    elems.foreach(loop)
    this
  }
}
