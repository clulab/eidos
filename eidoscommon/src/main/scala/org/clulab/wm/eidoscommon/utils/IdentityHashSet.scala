package org.clulab.wm.eidoscommon.utils

import scala.collection.JavaConverters._
import scala.collection.mutable

object IdentityHashSet {
  type IdentityHashSet[T <: AnyRef] = mutable.Set[T]

  def apply[T <: AnyRef](): IdentityHashSet[T] = {
    val jMap = new java.util.IdentityHashMap[T, java.lang.Boolean]
    val jSet = java.util.Collections.newSetFromMap[T](jMap)
    val sSet = jSet.asScala

    sSet
  }
}
