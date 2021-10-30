package org.clulab.wm.eidoscommon.utils

import scala.collection.mutable

object EqualityHashSet {
  type EqualityHashSet[K] = mutable.HashSet[K]
}
