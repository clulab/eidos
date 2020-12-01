package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.test.EidosTest

class TestMemory extends EidosTest {

  behavior of "memory"
  
  it should "work" in {
    val runtime = Runtime.getRuntime

    println(s"TotalMemory: ${runtime.totalMemory}")
    println(s" FreeMemory: ${runtime.freeMemory}")
    println(s"  MaxMemory: ${runtime.maxMemory}")
    println(s"  MAX_VALUE: ${Long.MaxValue}")
  }
}
