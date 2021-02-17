package org.clulab.wm.eidos.utils

import java.io.File

class ExitOnHeapExhaustion

object ExitOnHeapExhaustion {
  val lockFile = new File("exitOnHeapExhaustion.lock")

  def main(args: Array[String]): Unit = {
    println(this.getClass.getName)
    args.foreach(println)
    if (lockFile.exists) {
      lockFile.delete()
      var memoryList = List[Array[Int]]()
      while (true) {
        val arr = Array.fill(100000) { 42 }
        memoryList = arr :: memoryList
      }
    }
    else
      println("I am exiting normally.")
  }
}
