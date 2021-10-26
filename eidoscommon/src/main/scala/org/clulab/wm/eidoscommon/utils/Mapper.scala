package org.clulab.wm.eidoscommon.utils

import scala.collection.mutable

abstract class Mapper[K, V] {
  def put(key: K, value: V): Unit
  def get(key: K): V
  def getOrElse(key: K, default: => V): V
  def getValues: Seq[V]
  def size: Int
}

class EqualityMapper[K, V] extends Mapper[K, V] {
  protected val map = new mutable.HashMap[K, V]()

  def put(key: K, value: V): Unit = map.put(key, value)

  def get(key: K): V = map(key)

  def getOrElse(key: K, default: => V): V = map.getOrElse(key, default)

  def getValues: Seq[V] = map.values.toSeq

  def size: Int = map.size
}

class IdentityMapper[K <: AnyRef, V] extends Mapper[K, V] {
  protected val map = new IdentityHashMap[K, V]()

  def put(key: K, value: V): Unit = map(key) = value

  def get(key: K): V = map(key)

  def getOrElse(key: K, default: => V): V = map.getOrElse(key, default)

  def getValues: Seq[V] = map.values.toSeq

  def size: Int = map.size
}
