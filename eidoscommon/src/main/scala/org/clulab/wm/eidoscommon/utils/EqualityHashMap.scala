package org.clulab.wm.eidoscommon.utils

import scala.collection.mutable

object EqualityHashMap {
  type EqualityHashMap[K, V] = mutable.HashMap[K, V]
}
