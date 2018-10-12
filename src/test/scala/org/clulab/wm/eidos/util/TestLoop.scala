package org.clulab.wm.eidos.util

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.utils.Timer

class TestLoop extends Test {
  
  behavior of "Loop"
  
  ignore should "run faster in scala" in {
    val left = new Array[Float](300)
    left.indices.foreach(i => left(i) = 400 - i)
    val right = new Array[Float](300)
    right.indices.foreach(i => right(i) = -200 + i)
    val limit = 1000000

    val res1 = Timer.time("Using fold") {
      0.until(limit).foreach { i =>
        left.indices.foldRight(0.0f)((i, sum) => sum + left(i) * right(i))
      }
    }

    val res2 = Timer.time("Using fold") {
      0.until(limit).foreach { j =>
        var sum = 0.0f
        var i = 0
        while (i < left.length) {
          sum += left(i) * right(i)
          i += 1
        }
        sum
      }
    }
  }  
}