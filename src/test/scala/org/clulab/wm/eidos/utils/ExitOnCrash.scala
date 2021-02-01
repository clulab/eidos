package org.clulab.wm.eidos.utils

import edu.cmu.dynet.internal.{dynet_swig => dynet}
import org.clulab.fatdynet.utils.Initializer

import java.io.File

class ExitOnCrash

object ExitOnCrash {
  val lockFile = new File("exitOnCrash.lock")

  def main(args: Array[String]): Unit = {
    println(this.getClass.getName)
    args.foreach(println)
    if (lockFile.exists) {
      lockFile.delete()
      Initializer.initialize()
      dynet.writeNullPtr()
    }
    else
      println("I am exiting normally.")
  }
}
