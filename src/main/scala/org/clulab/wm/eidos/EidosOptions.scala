package org.clulab.wm.eidos

import org.clulab.wm.eidos.refiners.RefinerOptions

class EidosOptions(val cagRelevantOnly: Boolean) {
  val refinerOptions: RefinerOptions = RefinerOptions(cagRelevantOnly)
}

object EidosOptions {

  def apply(cagRelevantOnly: Boolean = true): EidosOptions = new EidosOptions(cagRelevantOnly)
}
