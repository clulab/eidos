package org.clulab.wm.eidos.utils

import java.io.File

class ExitOnDestroy

object ExitOnDestroy {
  val lockFile = new File("exitOnDestroy.lock")

  def main(args: Array[String]): Unit = {
    println(this.getClass.getName)
    args.foreach(println)
    if (lockFile.exists) {
      lockFile.delete()
      Thread.sleep(10000)
    }
    println("I am exiting normally.")
  }
}

