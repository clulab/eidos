package org.clulab.wm.eidos.portuguese.actions

import org.clulab.odin.{Mention, State, TextBoundMention}
import org.clulab.struct.Interval
import org.clulab.wm.eidos.actions.{AttachmentHandler, EidosBaseActions}

import scala.collection.mutable.ArrayBuffer

/** Actions for Portuguese **/
class PortugueseActions extends EidosBaseActions {

//  /** ??? */
//  def filterTriggers(mentions: Seq[Mention], state: State): Seq[Mention] = mentions.filterNot{ m =>
//    m.text.endsWith("ção")
//  }

  def checkOverlapWithAvoid(mentions: Seq[Mention], state: State): Seq[Mention] = {
    val (entities, other) = mentions.partition(_.matches("Entity"))
    // only keep those Entity mentions which do not overlap with an Avoid mention
    val validEntities = entities.flatMap{ m =>
      val overlapping = state.mentionsFor(m.sentence, m.tokenInterval)
      val triggers    = overlapping.filter(_.matches( label = "PossibleTrigger"))
      m match {
        // discard me.  I overlap with an avoid mention.
        // During expansion, we want to disallow overlap with *ANY* avoid,
        // but here we are more selective (we want to split around want to split triggers)
        case discard if overlapping.exists{om => om.matches("Avoid.*".r) && ! om.matches("PossibleTrigger")} =>
          Nil
        // trim me.  I overlap with a possible trigger.
        // FIXME: Implement this split!
        case trimEntity if triggers.nonEmpty =>
          var finalMentions : Seq[TextBoundMention] = Seq()
          // get set of trigger indexes
          val disallowed: Set[Int] = triggers.flatMap(_.tokenInterval).toSet
          val mentionSize =  m.end - m.start
          val tbm = m.asInstanceOf[TextBoundMention]
          // create a mast with ones [1,1,1,1,1]
          val mentionMask = ArrayBuffer.fill(mentionSize)(1)
          // mark disallowed elements [1,1,1,0,1]
          for (i <- disallowed){
            mentionMask.update(i-m.start, 0)
          }
          // for each for each sequence of 1's output a textbond mention
          // for example [1,1,1,0,1]
          // would output textbounds with intervals:
          // [0,3] and [4,5]
          var i=0
          while(i < mentionSize) {
            if(mentionMask(i) == 1){
              val start=i+m.start
              while(i < mentionSize && mentionMask(i) == 1){
                i=i+1
              }
              val end=i+m.start+1
              val newInterval = Interval(start, end)
              finalMentions = finalMentions ++ Seq(tbm.copy(tokenInterval = newInterval))
            }
            i=i+1
          }
          finalMentions.foreach(fm => println(fm.text))
          finalMentions
        case _ => Seq(m)
      }

    }
    (validEntities ++ other).distinct
  }

  /** identity action */
  def globalAction(mentions: Seq[Mention], state: State): Seq[Mention] = mentions

  override def keepMostCompleteEvents(mentions: Seq[Mention], state: State): Seq[Mention] = AttachmentHandler.keepMostCompleteEvents(mentions, state)

}
