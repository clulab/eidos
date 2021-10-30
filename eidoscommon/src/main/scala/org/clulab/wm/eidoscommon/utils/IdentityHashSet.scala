package org.clulab.wm.eidoscommon.utils

import scala.collection.JavaConverters._
import scala.collection.mutable

object IdentityHashSet {
  type IdentityHashSet[K] = mutable.Set[K]

  def apply[K](): mutable.Set[K] = {
    val jMap = new java.util.IdentityHashMap[K, java.lang.Boolean]
    val jSet = java.util.Collections.newSetFromMap[K](jMap)
    val sSet = jSet.asScala

    sSet
  }
}