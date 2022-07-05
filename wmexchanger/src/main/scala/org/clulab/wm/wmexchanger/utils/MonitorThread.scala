package org.clulab.wm.wmexchanger.utils

import com.typesafe.scalalogging.Logger

import java.util.Scanner

class MonitorThread(safeThread: SafeThread, logger: Logger, duration: Long) extends Thread {

  protected def stop(safeThread: SafeThread): Unit = {
    try {
      safeThread.userInterruption = true
      safeThread.interrupt
      safeThread.join(duration)
      if (safeThread.isAlive)
        safeThread.stop
    }
    catch {
      case _: InterruptedException =>
      case _: ThreadDeath =>
    }
  }

  def waitSafely(): Unit = {
    try {
      println("Press ENTER to exit...")
      new Scanner(System.in).nextLine()
      logger.info("User interruption")
      stop(safeThread)
      logger.info("Exiting...")
    }
    catch {
      case _: NoSuchElementException => logger.info("User input is unavailable.  Running without...")
      case _: Throwable => logger.info("Exiting...")
    }
  }

  override def run(): Unit = waitSafely()

  start
}
