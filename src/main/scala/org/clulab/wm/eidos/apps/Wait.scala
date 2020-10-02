package org.clulab.wm.eidos.apps

object Wait extends App {

  class Worker(val index: Int) {

    def run(): Unit = Wait.synchronized {
      println(s"Starting worker $index")
      Thread.sleep(2000)
      println(s"Stopping worker $index")
    }
  }

  Range.inclusive(1, 10).par.foreach { index =>
    new Worker(index).run()
  }
}
