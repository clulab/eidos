package org.clulab.wm.eidos.utils

import java.io.File

class ExitOnCrash

object ExitOnCrash {
  val lockFile = new File("exitOnCrash.lock")

  def main(args: Array[String]): Unit = {
    println(this.getClass.getName)
    args.foreach(println)
    if (lockFile.exists) {
      lockFile.delete()
    }
    else
      println("I am exiting normally.")
  }
}
