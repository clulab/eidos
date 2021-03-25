package org.clulab.wm.wmexchanger.utils

import java.util.Scanner

import org.apache.kafka.common.errors.InterruptException
import org.slf4j.Logger

abstract class SafeThread(logger: Logger) extends Thread {

  def runSafely(): Unit

  override def run(): Unit = {
    try {
      runSafely()
    }
    catch {
      case exception: InterruptException =>
        // This usually happens during consumer.poll().
        logger.info("Kafka interruption") // This is expected.
      case exception: InterruptedException =>
        logger.info("Java interruption") // This is expected.
      case exception: Throwable =>
        logger.error("Consumer interruption", exception)
    }
    finally {
      // This seems to be the only way to "cancel" the scanner.nextLine.
      System.exit(0)
    }
  }

  def waitSafely(duration: Long): Unit = SafeThread.waitSafely(this, logger, duration)

  start
}

object SafeThread {

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

  def waitSafely(thread: Thread, logger: Logger, duration: Long): Unit = {
    try {
      println("Press ENTER to exit...")
      new Scanner(System.in).nextLine()
      logger.info("User interruption")
      stop(thread, duration)
      logger.info("Exiting...")
    }
    catch {
      case _: Throwable => logger.info("Exiting...")
    }
  }
}
