package org.clulab.wm.eidos.utils

object Timer {

  // See http://biercoff.com/easily-measuring-code-execution-time-in-scala/
  def time[R](description: String)(block: => R): R = {
    println(description)
    val t0 = System.currentTimeMillis()
    val result: R = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println(s"Elapsed time: " + (t1 - t0) / 1000 + " sec.")
    result
  }
}
