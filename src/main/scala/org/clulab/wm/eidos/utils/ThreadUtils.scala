package org.clulab.wm.eidos.utils

import scala.collection.parallel.ForkJoinTasks

// For Scala 2.11, this is required.
//import scala.concurrent.forkjoin.ForkJoinPool

// For Scala 2.12, this is deprecated.
// import scala.concurrent.forkjoin.ForkJoinPool
// Alternative for Scala 2.12, which doesn't compile for 2.11.
//import java.util.concurrent.ForkJoinPool

object ThreadUtils {
  // This is a work-around for Scala version incompatibility.
  val forkJoinPoolConstructor = {
    // Get something of the right type.
    val defaultForkJoinPool = ForkJoinTasks.defaultForkJoinPool
    // Find the constructor.
    defaultForkJoinPool.getClass.getConstructor(classOf[Int])
  }

  def newForkJoinPool(threads: Int) = {
    // Invoke the constructor.
    forkJoinPoolConstructor.newInstance(threads.asInstanceOf[Integer])

    // For the record, this is the standard version
    //new ForkJoinPool(threads)
  }
}
