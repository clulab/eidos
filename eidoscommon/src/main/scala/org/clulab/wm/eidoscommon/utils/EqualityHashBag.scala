package org.clulab.wm.eidoscommon.utils

import org.clulab.wm.eidoscommon.utils.EqualityHashMap.EqualityHashMap

class EqualityHashBag[K](map: EqualityHashMap[K, Int]) extends HashBag[K](map) {

  def this() = this(new EqualityHashMap[K, Int]())
}

object EqualityHashBag {

  def apply[K](): EqualityHashBag[K] = new EqualityHashBag[K]()
}
