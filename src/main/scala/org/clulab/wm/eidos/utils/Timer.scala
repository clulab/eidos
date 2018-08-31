package org.clulab.wm.eidos.utils

import org.slf4j.LoggerFactory

object Timer {
  lazy val logger = LoggerFactory.getLogger(this.getClass())

  // See http://biercoff.com/easily-measuring-code-execution-time-in-scala/
  def time[R](description: String)(block: => R): R = {
    logger.info("Start: " + description)

    val t0 = System.currentTimeMillis()
    val result: R = block    // call-by-name
    val t1 = System.currentTimeMillis()

    logger.info("Stop: " + description)
    logger.info(s"Time: " + (t1 - t0) / 1000 + " sec.")
    result
  }
}
