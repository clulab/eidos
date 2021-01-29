package org.clulab.wm.eidos.utils

import scala.collection.JavaConverters._

class Launcher(val args: Array[String]) {

  def newProcessBuilder(args: Array[String]): ProcessBuilder = {
    val processBuilder = new ProcessBuilder(args.toList.asJava)

    processBuilder.redirectInput(ProcessBuilder.Redirect.INHERIT)
    processBuilder.redirectOutput(ProcessBuilder.Redirect.INHERIT)
    processBuilder.redirectError(ProcessBuilder.Redirect.INHERIT)
    processBuilder
  }

  def run(retry: Boolean): Int = {
    // If retry is true, runs the program until it returns 0.
    // If it is false, runs it just until it returns.
    // Either way, it returns the result of the program.
    var args = this.args
    var result = 0

    do {
      val processBuilder = newProcessBuilder(args)
      val process = processBuilder.start()

      result = process.waitFor()
      args = args.dropRight(1)
    } while (retry && result != 0 && args.length > 0)

    result
  }
}

object Launcher {

  def apply(args: Array[String]): Launcher = new Launcher(args)

  def main(args: Array[String]): Unit = {
    Launcher(args).run(true)
  }
}
