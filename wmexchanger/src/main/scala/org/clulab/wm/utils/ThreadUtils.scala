package org.clulab.wm.utils

object ThreadUtils {

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
