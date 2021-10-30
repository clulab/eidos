package org.clulab.wm.eidoscommon.utils

import org.clulab.wm.eidoscommon.utils.IdentityHashMap.IdentityHashMap

class IdentityHashBag[K](map: IdentityHashMap[K, Int]) extends HashBag[K](map) {

  def this() = this(IdentityHashMap[K, Int]())
}

object IdentityHashBag {

  def apply[K](): IdentityHashBag[K] = new IdentityHashBag[K]()
}
