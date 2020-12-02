package org.clulab.wm.eidoscommon.utils

import scala.annotation.tailrec

class TestLoop extends Test {
  
  behavior of "Loop"
  
  ignore should "run faster in scala" in {
    val left = new Array[Float](300)
    left.indices.foreach(i => left(i) = 400 - i)
    val right = new Array[Float](300)
    right.indices.foreach(i => right(i) = -200 + i)
    val limit = 10000000

    //    val res1 = Timer.time("Using fold") {
    //      0.until(limit).foreach { i =>
    //        left.indices.foldRight(0.0f)((i, sum) => sum + left(i) * right(i))
    //      }
    //    }

    // 261 ms
    val res2a = Timer.time("Using while a") {
      0.until(limit).foreach { j =>
        var sum = 0.0f // test
        var i = 0 // test
        while (i < left.length) {
          sum += left(i) * right(i)
          i += 1
        }
        sum
      }
    }

    // 70 ms
    val res2b = Timer.time("Using while b") {
      0.until(limit).foreach { j =>
        var sum = 0.0f // test
        var i = left.length - 1 // test

        while (i >= 0) {
          sum += left(i) * right(i)
          i -= 1
        }
        sum
      }
    }

    // 4122 ms
    val res3 = Timer.time("Using for") {
      0.until(limit).foreach { j =>
        var sum = 0.0f // test

        for (i <- 0 until left.length)
          sum += left(i) * right(i)
        sum
      }
    }

    // 4050 ms
    val res4 = Timer.time("Using recursion 1") {

      @tailrec
      def recSum(i: Int, sum: Float): Float =
        if (i < left.length) recSum(i + 1, sum + left(i) * right(i))
        else sum

      0.until(limit).foreach { j =>
        recSum(0, 0.0f)
      }
    }

//    val res5 = Timer.time("Using recursion 5") {
//
//      @tailrec
//      def recSum(i: Int): Float =
//        if (i >= left.length) 0.0f
//        else left(i) * right(i) + recSum(i + 1)
//
//      0.until(limit).foreach { j =>
//        recSum(0)
//      }
//    }
  }
}