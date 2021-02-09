package org.clulab.wm.eidos.utils

import java.io.File

class ExitAbnormally

object ExitAbnormally {
  val lockFile = new File("exitAbnormally.lock")

  def main(args: Array[String]): Unit = {
    println(this.getClass.getName)
    args.foreach(println)
    if (lockFile.exists) {
      lockFile.delete()
      println("I am exiting abnormally.")
      System.exit(1)
    }
    else
      println("I am exiting normally.")
  }
}
