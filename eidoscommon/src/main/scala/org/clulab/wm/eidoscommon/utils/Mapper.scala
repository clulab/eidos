package org.clulab.wm.eidoscommon.utils

import java.util

import scala.collection.JavaConverters._
import scala.collection.mutable

abstract class MentionMapper[K, V] {
  def put(key: K, value: V): Unit
  def get(key: K): V
  def getOrElse(key: K, default: => V): V
  def getValues: Seq[V]
  def size: Int
}

class EqualityMapper[K, V] extends MentionMapper[K, V] {
  protected val map = new mutable.HashMap[K, V]()

  def put(key: K, value: V): Unit = map.put(key, value)

  def get(key: K): V = map(key)

  def getOrElse(key: K, default: => V): V = map.getOrElse(key, default)

  def getValues: Seq[V] = map.values.toSeq

  def size: Int = map.size
}

class IdentityMapper[K, V] extends MentionMapper[K, V] {
  protected val map = new util.IdentityHashMap[K, V]()

  def put(key: K, value: V): Unit = map.put(key, value)

  def get(key: K): V = Option(map.get(key)).getOrElse(throw new Exception(s"Value for key $key not found."))

  def getOrElse(key: K, default: => V): V = Option(map.get(key)).getOrElse(default)

  def getValues: Seq[V] = map.values.asScala.toSeq

  def size: Int = map.size
}
