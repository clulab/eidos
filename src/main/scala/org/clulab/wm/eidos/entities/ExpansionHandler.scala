package org.clulab.wm.eidos.entities

import org.clulab.odin._

// Unopinionated trait for "entity" expansions
trait ExpansionHandler {

  def expandArguments(mentions: Seq[Mention], state: State): Seq[Mention]

  def expand(entity: Mention, maxHops: Int, stateFromAvoid: State): Mention
}
