package org.clulab.wm.eidos.utils

class ExitNormally

object ExitNormally {

  def main(args: Array[String]): Unit = {
    println(this.getClass.getName)
    args.foreach(println)
    println("I am exiting normally.")
  }
}

