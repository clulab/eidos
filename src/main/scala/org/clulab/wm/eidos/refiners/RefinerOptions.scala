package org.clulab.wm.eidos.refiners

class RefinerOptions(val cagRelevantOnly: Boolean) {
}

object RefinerOptions {
  lazy val irrelevant: RefinerOptions = RefinerOptions()

  def apply(cagRelevantOnly: Boolean = true): RefinerOptions = new RefinerOptions(cagRelevantOnly)
}
