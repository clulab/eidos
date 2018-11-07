package org.clulab.wm.eidos.actions

import org.clulab.odin._

// Unopinionated trait for "entity" expansions
trait ExpansionHandler {

  def expandArguments(mentions: Seq[Mention], state: State): Seq[Mention]
}
