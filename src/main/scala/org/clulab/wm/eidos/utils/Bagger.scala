package org.clulab.wm.eidos.utils

import java.util

import scala.collection.mutable

abstract class Bagger[T] {
  def put(values: Seq[T]): Bagger[T]
  def putIfNew(value: T, block: => Unit): Unit
  def get(): Seq[T]
  def size: Int
}

class HashCodeBagger[T] extends Bagger[T] {
  protected val map = new mutable.HashMap[T, Int]()

  def put(values: Seq[T]): HashCodeBagger[T] = { values.foreach(putIfNew(_, ())); this }

  def putIfNew(value: T, block: => Unit): Unit = {
    val count = map.getOrElse(value, 0)
    
    map.put(value, count + 1)
    if (count == 0)
      block
  }

  def get(): Seq[T] = map.keySet.toSeq

  def size: Int = map.size
}

class IdentityBagger[T] extends Bagger[T] {
  protected val map = new util.IdentityHashMap[T, Int]()

  def put(values: Seq[T]): IdentityBagger[T] = { values.foreach(putIfNew(_, ())); this }

  def putIfNew(value: T, block: => Unit): Unit =
    if (map.containsKey(value))
      map.put(value, map.get(value) + 1)
    else {
      map.put(value, 1)
      block
    }

  def get(): Seq[T] = map.keySet().toArray.toSeq.map(_.asInstanceOf[T])

  def size: Int = map.size
}
