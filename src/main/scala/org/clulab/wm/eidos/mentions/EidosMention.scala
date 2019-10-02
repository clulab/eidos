package org.clulab.wm.eidos.mentions

import java.util

import org.clulab.odin._
import org.clulab.wm.eidos.groundings._
import org.clulab.struct.Interval
import org.clulab.wm.eidos.utils.HashCodeBagger
import org.clulab.wm.eidos.utils.IdentityBagger

import scala.collection.mutable

abstract class MentionMapper {
  def put(odinMention: Mention, eidosMention: EidosMention): Unit
  def getOrElse(odinMention: Mention, default: => EidosMention): EidosMention
}

class HashCodeMapper extends MentionMapper {
  protected val mapOfMentions = new mutable.HashMap[Mention, EidosMention]()

  def put(odinMention: Mention, eidosMention: EidosMention): Unit = mapOfMentions.put(odinMention, eidosMention)

  def getOrElse(odinMention: Mention, default: => EidosMention): EidosMention = mapOfMentions.getOrElse(odinMention, default)
}

class IdentityMapper extends MentionMapper {
  protected val mapOfMentions = new util.IdentityHashMap[Mention, EidosMention]()

  def put(odinMention: Mention, eidosMention: EidosMention): Unit = mapOfMentions.put(odinMention, eidosMention)

  def getOrElse(odinMention: Mention, default: => EidosMention): EidosMention =
      if (mapOfMentions.containsKey(odinMention)) mapOfMentions.get(odinMention)
      else default
}


abstract class EidosMention(val odinMention: Mention, mentionMapper: MentionMapper) /* extends Mention if really needs to */ {
  type StringAndStart = (String, Int)

  // This must happen before the remap in case arguments point back to this
  mentionMapper.put(odinMention, this)

  // Accessor method to facilitate cleaner code downstream
  val label: String = odinMention.label

  // Convenience function for parallel construction
  val odinArguments: Map[String, Seq[Mention]] = odinMention.arguments

  // Access to new and improved Eidos arguments
  val eidosArguments: Map[String, Seq[EidosMention]] = remapOdinArguments(odinArguments, mentionMapper)

  protected def remapOdinArguments(odinArguments: Map[String, Seq[Mention]],
      mentionMapper: MentionMapper): Map[String, Seq[EidosMention]] = {
    odinArguments.mapValues(odinMentions => EidosMention.asEidosMentions(odinMentions, mentionMapper))
  }

  protected def remapOdinMention(odinMention: Mention, mentionMapper: MentionMapper): EidosMention =
      EidosMention.asEidosMentions(Seq(odinMention), mentionMapper).head

  /* Methods for canonicalForms of Mentions */

  // Return any mentions that are involved in the canonical name.  By default, the argument values.
  // This is here to allow subclasses to override it so that the Canonicalizer doesn't have to keep track.
  def canonicalMentions: Seq[Mention] = odinArguments.values.flatten.toSeq

  var canonicalName: Option[String] = None
  var groundings: Option[OntologyAliases.OntologyGroundings] = None

  // Other EidosMentions which can be reached from this.
  def reachableMentions: Seq[EidosMention] = eidosArguments.values.flatten.toSeq

  // Some way to calculate or store these, possibly in subclass
  def tokenIntervals: Seq[Interval] = Seq(odinMention.tokenInterval)
  def negation: Boolean = ???
}

object EidosMention {

  def newEidosMention(odinMention: Mention, mentionMapper: MentionMapper): EidosMention = {
    odinMention match {
      case mention: TextBoundMention => new EidosTextBoundMention(mention, mentionMapper)
      case mention: EventMention => new EidosEventMention(mention, mentionMapper)
      case mention: RelationMention => new EidosRelationMention(mention, mentionMapper)
      case mention: CrossSentenceMention => new EidosCrossSentenceMention(mention, mentionMapper)
      case _ => throw new IllegalArgumentException("Unknown Mention: " + odinMention)
    }
  }

  def asEidosMentions(odinMentions: Seq[Mention], mentionMapper: MentionMapper): Seq[EidosMention] = {
    val eidosMentions = odinMentions.map { odinMention =>
      mentionMapper.getOrElse(odinMention, newEidosMention(odinMention, mentionMapper))
    }
    eidosMentions
  }

  def asEidosMentions(odinMentions: Seq[Mention]): Seq[EidosMention] = {
    // This will map odinMentions to eidosMentions.
    val mentionMapper = new HashCodeMapper()
    //  val mentionMapper = new IdentityMapper()

    asEidosMentions(odinMentions, mentionMapper)
  }

  def findReachableOdinMentions(surfaceMentions: Seq[Mention]): Seq[Mention] = {
    // Using the hash code results in value comparisons that removes duplicates.
    val mentionBagger = new HashCodeBagger[Mention]()
    // val mentionBagger = new IdentityBagger[Mention]()

    def addMention(odinMention: Mention): Unit = {
      mentionBagger.putIfNew(odinMention, {
        odinMention.arguments.flatMap(_._2).foreach(addMention)
        // Skipping paths
        odinMention match {
          case eventMention: EventMention =>
            addMention(eventMention.trigger)
          case crossSentenceMention: CrossSentenceMention =>
            addMention(crossSentenceMention.anchor)
            addMention(crossSentenceMention.neighbor)
          case _ =>
        }
      })
    }

    surfaceMentions.foreach(addMention)
    mentionBagger.get()
  }

  def findUnderlyingOdinMentions(surfaceMentions: Seq[Mention]): Seq[Mention] = {
    val reachableMentions = findReachableOdinMentions(surfaceMentions)
    val underlyingMentions = reachableMentions.filter { reachableMention =>
      !surfaceMentions.exists { surfaceMention =>
        surfaceMention.eq(reachableMention)
      }
    }

    underlyingMentions
  }

  def findReachableEidosMentions(surfaceMentions: Seq[EidosMention]): Seq[EidosMention] = {
    // For the EidosMentions, identity should be used because it is faster
    // and the underlying Odin mentions are known to be distinct.
    val mentionBagger = new IdentityBagger[EidosMention]()

    def addMention(eidosMention: EidosMention): Unit = {
      mentionBagger.putIfNew(eidosMention, {
        eidosMention.reachableMentions.foreach(addMention)
      })
    }

    surfaceMentions.foreach(addMention)
    mentionBagger.get()
  }

  def findUnderlyingEidosMentions(surfaceMentions: Seq[EidosMention]): Seq[EidosMention] = {
    val reachableMentions = findReachableEidosMentions(surfaceMentions)
    val underlyingMentions = reachableMentions.filter { reachableMention =>
      !surfaceMentions.exists { surfaceMention =>
        surfaceMention.eq(reachableMention)
      }
    }

    underlyingMentions
  }

  def hasUnderlyingMentions(surfaceMentions: Seq[Mention]): Boolean = findUnderlyingOdinMentions(surfaceMentions).nonEmpty

  def before(left: EidosMention, right: EidosMention): Boolean = {
    val leftSentence = left.odinMention.sentence
    val rightSentence = right.odinMention.sentence

    if (leftSentence != rightSentence)
      leftSentence < rightSentence
    else {
      val leftStart = left.odinMention.start
      val rightStart = right.odinMention.start

      if (leftStart != rightStart)
        leftStart < rightStart
      else {
        val leftEnd = left.odinMention.end
        val rightEnd = right.odinMention.end

        if (leftEnd != rightEnd)
          leftEnd < rightEnd
        else
          true
      }
    }
  }
}

class EidosTextBoundMention(val odinTextBoundMention: TextBoundMention, mentionMapper: MentionMapper)
    extends EidosMention(odinTextBoundMention, mentionMapper) {

  override def canonicalMentions: Seq[Mention] = Seq(odinMention)
}

class EidosEventMention(val odinEventMention: EventMention, mentionMapper: MentionMapper)
    extends EidosMention(odinEventMention, mentionMapper) {

  val odinTrigger: TextBoundMention = odinEventMention.trigger

  val eidosTrigger: EidosMention = remapOdinMention(odinTrigger, mentionMapper)

  override def canonicalMentions: Seq[Mention] =
      super.canonicalMentions ++ Seq(odinTrigger)

  override def reachableMentions: Seq[EidosMention] = super.reachableMentions ++ Seq(eidosTrigger)
}

class EidosRelationMention(val odinRelationMention: RelationMention, mentionMapper: MentionMapper)
    extends EidosMention(odinRelationMention, mentionMapper) {
}

class EidosCrossSentenceMention(val odinCrossSentenceMention: CrossSentenceMention, mentionMapper: MentionMapper)
    extends EidosMention(odinCrossSentenceMention, mentionMapper) {

  val odinAnchor: Mention = odinCrossSentenceMention.anchor

  val eidosAnchor: EidosMention = remapOdinMention(odinAnchor, mentionMapper)

  val odinNeighbor: Mention = odinCrossSentenceMention.neighbor

  val eidosNeighbor: EidosMention = remapOdinMention(odinNeighbor, mentionMapper)

  override def canonicalMentions: Seq[Mention] =
    Seq(odinAnchor, odinNeighbor)

  override def reachableMentions: Seq[EidosMention] = super.reachableMentions ++ Seq(eidosAnchor, eidosNeighbor)
}
