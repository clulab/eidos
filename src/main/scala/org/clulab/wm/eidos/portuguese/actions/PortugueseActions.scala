package org.clulab.wm.eidos.portuguese.actions

import org.clulab.odin.{Actions, Mention, State}

class PortugueseActions extends Actions {

  def checkOverlapWithAvoid(mentions: Seq[Mention], state: State): Seq[Mention] = {
    val (entities, other) = mentions.partition(_.matches("Entity"))
    // only keep those Entity mentions which do not overlap with an Avoid mention
    val validEntities = entities.filter{ m =>
      state.mentionsFor(m.sentence, m.tokenInterval).filter{_.matches("Avoid.*".r)}.isEmpty
    }
    (validEntities ++ other).distinct
  }

}
