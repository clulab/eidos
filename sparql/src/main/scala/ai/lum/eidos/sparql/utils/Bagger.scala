package ai.lum.eidos.sparql.utils

import java.util

import scala.collection.mutable

abstract class Bagger[T] {
  def put(values: Seq[T]): Bagger[T] = { values.foreach(put); this }
  def put(value: T): Bagger[T]
  def putIfNew(value: T, block: => Unit): Unit
  def get(): Seq[T]
  def get(key: T): Int
  def size: Int
}

class HashCodeBagger[T] extends Bagger[T] {
  protected val map = new mutable.HashMap[T, Int]()

  def put(value: T): HashCodeBagger[T] = { putIfNew(value, ()); this }

  def putIfNew(value: T, block: => Unit): Unit = {
    val count = map.getOrElse(value, 0)

    if (count == 0)
      block
    map.put(value, count + 1)
  }

  def get(): Seq[T] = map.keySet.toSeq

  def get(key: T): Int = map(key)

  def size: Int = map.size
}

class IdentityBagger[T] extends Bagger[T] {
  protected val map = new util.IdentityHashMap[T, Int]()

  def put(value: T): IdentityBagger[T] = { putIfNew(value, ()); this }

  def putIfNew(value: T, block: => Unit): Unit =
    if (map.containsKey(value))
      map.put(value, map.get(value) + 1)
    else {
      map.put(value, 1)
      block
    }

  def get(): Seq[T] = map.keySet().toArray.toSeq.map(_.asInstanceOf[T])

  def get(key: T): Int = map.get(key)

  def size: Int = map.size
}
