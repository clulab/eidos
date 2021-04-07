package org.clulab.wm.wmexchanger.utils

import org.clulab.wm.eidoscommon.utils.Logging

trait LoopApp extends Logging {

  // Returns whether to continue looping or not.
  def safeLoop(f: () => SafeThread): Boolean = {
    try {
      val thread = f()
      thread.join()
      !(thread.userInterruption || thread.isInterrupted)
    }
    catch {
      case throwable: Throwable =>
        logger.error("", throwable)
        true
    }
  }

  def loop(f: () => SafeThread): Unit = {
    // Keep looping until true is returned, indicating normal loop termination.
    while (safeLoop(f)) Thread.sleep(5000)
  }
}
