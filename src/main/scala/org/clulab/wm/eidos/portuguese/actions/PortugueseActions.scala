package org.clulab.wm.eidos.portuguese.actions

import org.clulab.odin.{Mention, State}
import org.clulab.wm.eidos.actions.EidosBaseActions

/** Actions for Portuguese **/
class PortugueseActions extends EidosBaseActions {

  def checkOverlapWithAvoid(mentions: Seq[Mention], state: State): Seq[Mention] = {
    val (entities, other) = mentions.partition(_.matches("Entity"))
    // only keep those Entity mentions which do not overlap with an Avoid mention
    val validEntities = entities.filter{ m =>
      val overlapping = state.mentionsFor(m.sentence, m.tokenInterval)
      ! overlapping.exists{_.matches("Avoid.*".r)}
    }
    (validEntities ++ other).distinct
  }

  /** identity action */
  def globalAction(mentions: Seq[Mention], state: State): Seq[Mention] = mentions


  // FIXME: Any fancy filtering needed?
  override def keepMostCompleteEvents(mentions: Seq[Mention], state: State): Seq[Mention] = mentions

}
