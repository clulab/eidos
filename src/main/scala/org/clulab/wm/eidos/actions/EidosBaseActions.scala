package org.clulab.wm.eidos.actions

import org.clulab.odin.{Actions, Mention, State}

/** All *event* actions in Eidos must implement this trait */
trait EidosBaseActions extends Actions {

  def globalAction(mentions: Seq[Mention], state: State): Seq[Mention]

  def keepMostCompleteEvents(mentions: Seq[Mention], state: State): Seq[Mention]

}
