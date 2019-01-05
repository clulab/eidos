package org.clulab.wm.eidos.apps

object ExitCode {

  def main(args: Array[String]) {
    val exitCode = args(0).toInt
    println(s"Returning $exitCode")

    System.exit(exitCode)
  }
}
