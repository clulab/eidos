package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.test.TestUtils._
import org.slf4j.Logger
import org.slf4j.LoggerFactory

class TestMemory extends Test {

  behavior of "memory"
  
  it should "work" in {
    val runtime = Runtime.getRuntime

    println(s"TotalMemory: ${runtime.totalMemory}")
    println(s" FreeMemory: ${runtime.freeMemory}")
    println(s"  MaxMemory: ${runtime.maxMemory}")
    println(s"  MAX_VALUE: ${Long.MaxValue}")
  }
}
