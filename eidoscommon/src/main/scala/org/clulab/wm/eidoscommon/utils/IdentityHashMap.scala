package org.clulab.wm.eidoscommon.utils

import scala.collection.JavaConverters._
import scala.collection.mutable

object IdentityHashMap {
  type IdentityHashMap[K <: AnyRef, V] = mutable.Map[K, V]

  def apply[K <: AnyRef, V](): IdentityHashMap[K, V] = {
    val jMap = new java.util.IdentityHashMap[K, V]
    val sMap = jMap.asScala

    sMap
  }
}