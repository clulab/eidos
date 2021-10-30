package org.clulab.wm.eidoscommon.utils

import scala.collection.mutable

class HashBag[T](protected val map: mutable.Map[T, Int]) extends mutable.Set[T] {

  override def +=(elem: T): HashBag.this.type = {
    val count = map.getOrElse(elem, 0) + 1

    map(elem) = count
    this
  }

  override def -=(elem: T): HashBag.this.type = {
    val count = map.getOrElse(elem, 0) - 1

    if (count > 0)
      map(elem) = count
    else
      map.remove(elem)
    this
  }

  override def contains(elem: T): Boolean = map.contains(elem)

  override def iterator: Iterator[T] = map.keysIterator

  def count(elem: T): Int = map(elem)

  def ++(elems: Seq[T]): HashBag.this.type = {
    elems.foreach { elem => this + elem }
    this
  }

  def ++(elems: Seq[T], getNeighbors: T => Iterable[T]): HashBag.this.type = {
    elems.foreach { elem => this ++ getNeighbors(elem) }
    this
  }
}
