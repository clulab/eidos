package org.clulab.wm.eidos.refiners

import org.clulab.wm.eidos.utils.Timer

class Refiner(val name: String) {

  def time[T](verbose: Boolean)(block: => T): T = {
    val result = Timer.time("Run " + name, verbose) {
      block
    }
    result
  }
}
