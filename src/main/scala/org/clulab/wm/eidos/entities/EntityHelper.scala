package org.clulab.wm.eidos.entities

import org.clulab.odin.{Mention, TextBoundMention}
import org.clulab.struct.Interval

import scala.annotation.tailrec

object EntityHelper {
  /***
    * Recursively splits a TextBoundMention (flat) on likely coordinations.
    */
  @tailrec
  protected def splitCoordinatedEntities(m: TextBoundMention, entities: Seq[Mention]): Seq[Mention] = {

    val coordIndex: Option[Int] = m.tokenInterval.find(i => isCoord(i, m))

    coordIndex match {
      // No coordination
      case None => entities ++ List(m)
      // mention contains only CC
      case Some(skipTok) if skipTok == m.start && m.end == m.start + 1 =>
        entities ++ List(m)
      // mention begins with CC, then skip this token and advance one
      case Some(skipTok) if skipTok == m.start =>
        val remaining = m.copy(tokenInterval = Interval(skipTok + 1, m.end))
        splitCoordinatedEntities(remaining, entities)
      // mention ends with CC, then discard and return
      case Some(skipTok) if skipTok == m.end - 1 =>
        val chunk = List(m.copy(tokenInterval = Interval(m.start, skipTok)))
        entities ++ chunk
      // otherwise, we need to split again
      case Some(idx) =>
        val chunk = if (m.start == idx) Nil else List(m.copy(tokenInterval = Interval(m.start, idx)))
        val remaining = m.copy(tokenInterval = Interval(idx + 1, m.end))
        splitCoordinatedEntities(remaining, entities ++ chunk)
    }
  }

  def splitCoordinatedEntities(m: Mention): Seq[Mention] = m match {
    case tb: TextBoundMention => splitCoordinatedEntities(tb, Nil)
    case _ => Seq(m)
  }

  /** Checks if the indexed token is a coordination **/
  def isCoord(i: Int, m: Mention): Boolean = EntityConstraints.isCoord(i, m)

}
