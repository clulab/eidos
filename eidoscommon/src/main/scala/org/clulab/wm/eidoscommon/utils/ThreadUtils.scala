package org.clulab.wm.eidoscommon.utils

import scala.collection.parallel.{ForkJoinTaskSupport, ForkJoinTasks, ParSeq}

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

  def parallelize[T](seq: Seq[T], threads: Int): ParSeq[T] = {
    val forkJoinPool = newForkJoinPool(threads)
    val forkJoinTaskSupport = new ForkJoinTaskSupport(forkJoinPool)
    val parSeq = seq.par

    parSeq.tasksupport = forkJoinTaskSupport
    parSeq
  }

  def stop(thread: Thread, duration: Long): Unit = {
    try {
      thread.interrupt
      thread.join(duration)
      if (thread.isAlive)
        thread.stop
    }
    catch {
      case _: InterruptedException =>
      case _: ThreadDeath =>
    }
  }
}
