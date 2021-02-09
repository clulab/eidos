package org.clulab.wm.eidos.utils

import edu.cmu.dynet.internal.{dynet_swig => dynet}
import org.clulab.fatdynet.utils.Initializer

import java.io.File

class ExitOnSignal

object ExitOnSignal {
  val lockFile = new File("exitOnSignal.lock")

  def main(args: Array[String]): Unit = {
    println(this.getClass.getName)
    args.foreach(println)
    if (lockFile.exists) {
      lockFile.delete()
      Initializer.initialize()
      dynet.raiseSignal(9) // kill which can't be caught
    }
    else
      println("I am exiting normally.")
  }
}
