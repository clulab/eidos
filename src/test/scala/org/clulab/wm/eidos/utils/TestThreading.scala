package org.clulab.wm.eidos.utils

import org.clulab.wm.eidos.test.TestUtils._

import scala.collection.parallel.ForkJoinTaskSupport

class TestThreading extends Test {
  val threads = 26
  val numbers = 0.until(threads)
  val parNumbers = numbers.par
  val forkJoinPool = ThreadUtils.newForkJoinPool(threads)
  val forkJoinTaskSupport = new ForkJoinTaskSupport(forkJoinPool)

  // This should compile and output is not in order.
  parNumbers.tasksupport = forkJoinTaskSupport
  parNumbers.foreach { number =>
    println(number)
  }
}