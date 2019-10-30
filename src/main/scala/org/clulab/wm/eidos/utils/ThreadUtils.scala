package org.clulab.wm.eidos.utils

// For Scala 2.11, this is required.
import scala.concurrent.forkjoin.ForkJoinPool

// For Scala 2.12, this is deprecated.
// import scala.concurrent.forkjoin.ForkJoinPool
// Alternative for Scala 2.12, which doesn't compile for 2.11.
//import java.util.concurrent.ForkJoinPool

object ThreadUtils {

  def newForkJoinPool(threads: Int) = new ForkJoinPool(threads)
}
