package org.clulab.wm.eidos.utils

import org.clulab.odin.CrossSentenceMention
import org.clulab.odin.EventMention
import org.clulab.odin.Mention

object OdinMention {

  def getNeighbors(odinMention: Mention): Iterable[Mention] = {
    val arguments = odinMention.arguments.flatMap(_._2)

    // Skip paths!
    odinMention match {
      case eventMention: EventMention =>
        arguments ++ List(eventMention.trigger)
      case crossSentenceMention: CrossSentenceMention =>
        // These are usually duplicated in the arguments, but just in case...
        arguments ++ List(crossSentenceMention.anchor, crossSentenceMention.neighbor)
      case _ => arguments
    }
  }

  // The point of this is to avoid visiting any mention twice if there are any loops.
  // Equality is eq() and not == here, in part so that mentions are not constantly compared.
  def findAllByIdentity(surfaceMentions: Seq[Mention]): Seq[Mention] =
    IdentityBagger[Mention](surfaceMentions, mention => getNeighbors(mention) ).get

  // In these case we're not caring which of the mentions are used if they are ==.
  // Because this equality is defined loosely, the mentions may still differ, for
  // instance in the foundBy field.
  def findAllByEquality(surfaceMentions: Seq[Mention]): Seq[Mention] =
    findAllByIdentity(surfaceMentions).distinct
}
