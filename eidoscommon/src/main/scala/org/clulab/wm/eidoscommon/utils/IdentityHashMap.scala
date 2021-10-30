package org.clulab.wm.eidoscommon.utils

import scala.collection.JavaConverters._
import scala.collection.mutable

object IdentityHashMap {
  type IdentityHashMap[K, V] = mutable.HashMap[K, V]

  def apply[K, V](): mutable.Map[K, V] = {
    val jMap = new java.util.IdentityHashMap[K, V]
    val sMap = jMap.asScala

    sMap
  }
}