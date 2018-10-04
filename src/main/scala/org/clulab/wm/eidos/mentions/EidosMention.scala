package org.clulab.wm.eidos.mentions

import java.util.IdentityHashMap

import org.clulab.odin._
import org.clulab.wm.eidos.groundings._
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.EidosAttachment
import org.clulab.wm.eidos.utils.Canonicalizer

import scala.collection.mutable.HashMap

abstract class MentionMapper {
  def put(odinMention: Mention, eidosMention: EidosMention): Unit
  def getOrElse(odinMention: Mention, default: => EidosMention): EidosMention
}

class HashCodeMapper extends MentionMapper {
  protected val mapOfMentions = new HashMap[Mention, EidosMention]()

  def put(odinMention: Mention, eidosMention: EidosMention): Unit = mapOfMentions.put(odinMention, eidosMention)

  def getOrElse(odinMention: Mention, default: => EidosMention): EidosMention = mapOfMentions.getOrElse(odinMention, default)
}

class IdentityMapper extends MentionMapper {
  protected val mapOfMentions = new IdentityHashMap[Mention, EidosMention]()

  def put(odinMention: Mention, eidosMention: EidosMention): Unit = mapOfMentions.put(odinMention, eidosMention)

  def getOrElse(odinMention: Mention, default: => EidosMention): EidosMention =
      if (mapOfMentions.containsKey(odinMention)) mapOfMentions.get(odinMention)
      else default
}

abstract class Bagger[T] {
  def put(values: Seq[T]): Bagger[T]
  def putIfNew(value: T, block: => Unit): Unit
  def get(): Seq[T]
  def size: Int
}

class HashCodeBagger[T] extends Bagger[T] {
  protected val map = new HashMap[T, Int]()

  def put(values: Seq[T]): HashCodeBagger[T] = { values.foreach(putIfNew(_, ())); this }

  def putIfNew(value: T, block: => Unit): Unit = {
    val count = map.getOrElse(value, 0)

    if (count == 0)
      block
    map.put(value, count + 1)
  }

  def get(): Seq[T] = map.keySet.toSeq

  def size: Int = map.size
}

class IdentityBagger[T] extends Bagger[T] {
  protected val map = new IdentityHashMap[T, Int]()

  def put(values: Seq[T]): IdentityBagger[T] = { values.foreach(putIfNew(_, ())); this }

  def putIfNew(value: T, block: => Unit): Unit =
    if (map.containsKey(value))
      map.put(value, map.get(value) + 1)
    else {
      map.put(value, 1)
      block
    }

  def get(): Seq[T] = map.keySet().toArray.toSeq.map(_.asInstanceOf[T])

  def size: Int = map.size
}

abstract class EidosMention(val odinMention: Mention, canonicalizer: Canonicalizer, ontologyGrounder: MultiOntologyGrounder,
    mentionMapper: MentionMapper) /* extends Mention if really needs to */ {
  type StringAndStart = (String, Int)

  // This must happen before the remap in case arguments point back to this
  mentionMapper.put(odinMention, this)

  // Accessor method to facilitate cleaner code downstream
  val label = odinMention.label

  // Convenience function for parallel construction
  val odinArguments: Map[String, Seq[Mention]] = odinMention.arguments

  // Access to new and improved Eidos arguments
  val eidosArguments: Map[String, Seq[EidosMention]] = remapOdinArguments(odinArguments, canonicalizer, ontologyGrounder, mentionMapper)

  val eidosMentionsFromAttachments: Seq[EidosMention] = {
    val attachmentMentions = odinMention.attachments.toSeq.flatMap(_.asInstanceOf[EidosAttachment].attachmentMentions)

    EidosMention.asEidosMentions(attachmentMentions, canonicalizer, ontologyGrounder, mentionMapper)
  }

  protected def remapOdinArguments(odinArguments: Map[String, Seq[Mention]], canonicalizer: Canonicalizer, ontologyGrounder: MultiOntologyGrounder,
      mentionMapper: MentionMapper): Map[String, Seq[EidosMention]] = {
    odinArguments.mapValues(odinMentions => EidosMention.asEidosMentions(odinMentions, canonicalizer, ontologyGrounder, mentionMapper))
  }

  protected def remapOdinMention(odinMention: Mention, canonicalizer: Canonicalizer, ontologyGrounder: MultiOntologyGrounder, mentionMapper: MentionMapper): EidosMention =
      EidosMention.asEidosMentions(Seq(odinMention), canonicalizer, ontologyGrounder, mentionMapper)(0)

  // This is lazy because canonicalMentions is called and that may be overridden in the derived class.
  // The overriden method will not be called in this constructor.
  lazy val canonicalName: String = {
    // Sentence has been added to account for cross sentence mentions.
    def lessThan(left: Mention, right: Mention): Boolean =
        if (left.sentence != right.sentence)
          left.sentence < right.sentence
        else if (left.start != right.start)
          left.start < right.start
        // This one shouldn't really be necessary.
        else if (left.end != right.end)
          left.end < right.end
        else
          false // False is needed to preserve order on tie.

    canonicalMentions.sortWith(lessThan).map(canonicalFormSimple).mkString(" ")
  }

  // Return any mentions that are involved in the canonical name.  By default, the argument values.
  protected def canonicalMentions: Seq[Mention] = odinArguments.values.flatten.toSeq

  // This is similarly lazy because groundOntology calls canonicalName.
  lazy val grounding: Map[String, OntologyGrounding] = ontologyGrounder.groundOntology(this)

  // Some way to calculate or store these, possibly in subclass
  def tokenIntervals: Seq[Interval] = Seq(odinMention.tokenInterval)
  def negation: Boolean = ???

  /* Methods for canonicalForms of Mentions */
  protected def canonicalFormSimple(m: Mention): String = {
    val lemmas = m.lemmas.get
    val tags = m.tags.get
    val ners = m.entities.get
    val contentLemmas = for {
      i <- lemmas.indices
      if canonicalizer.isCanonical(lemmas(i), tags(i), ners(i))
    } yield lemmas(i)

    if (contentLemmas.isEmpty)
      m.text // fixme -- better and cleaner backoff
    else
      contentLemmas.mkString(" ").trim.replaceAll(" +", " ")
//    println("  * result: " + contentLemmas.mkString(" "))
  }
}

object EidosMention {

  protected def newMentionMapper() = new HashCodeMapper()
//  protected def newMentionMapper() = new IdentityMapper()

  protected def newMentionBagger() = new HashCodeBagger[Mention]()
//  protected def newMentionBagger() = new IdentityBagger[Mention]()

  def newEidosMention(odinMention: Mention, canonicalizer: Canonicalizer, ontologyGrounder: MultiOntologyGrounder,
      mentionMapper: MentionMapper): EidosMention = {
    odinMention match {
      case mention: TextBoundMention => new EidosTextBoundMention(mention, canonicalizer, ontologyGrounder, mentionMapper)
      case mention: EventMention => new EidosEventMention(mention, canonicalizer, ontologyGrounder, mentionMapper)
      case mention: RelationMention => new EidosRelationMention(mention, canonicalizer, ontologyGrounder, mentionMapper)
      case mention: CrossSentenceMention => new EidosCrossSentenceMention(mention, canonicalizer, ontologyGrounder, mentionMapper)
      case _ => throw new IllegalArgumentException("Unknown Mention: " + odinMention)
    }
  }

  def asEidosMentions(odinMentions: Seq[Mention], canonicalizer: Canonicalizer, ontologyGrounder: MultiOntologyGrounder,
      mentionMapper: MentionMapper): Seq[EidosMention] = {
    val eidosMentions = odinMentions.map { odinMention =>
      mentionMapper.getOrElse(odinMention, newEidosMention(odinMention, canonicalizer, ontologyGrounder, mentionMapper))
    }
    eidosMentions
  }

  def asEidosMentions(odinMentions: Seq[Mention], canonicalizer: Canonicalizer, ontologyGrounder: MultiOntologyGrounder): Seq[EidosMention] =
      // One could optionally keep this map around
      asEidosMentions(odinMentions, canonicalizer, ontologyGrounder, newMentionMapper()): Seq[EidosMention]

  def findReachableMentions(surfaceMentions: Seq[Mention]): Seq[Mention] = {
    val mentionBagger = newMentionBagger()

    def addMention(odinMention: Mention): Unit = {
      mentionBagger.putIfNew(odinMention, {
        odinMention.arguments.flatMap(_._2).foreach(addMention)
        // Skipping paths
        odinMention.attachments.asInstanceOf[Set[EidosAttachment]].flatMap(_.attachmentMentions).foreach(addMention)
        if (odinMention.isInstanceOf[EventMention])
          addMention(odinMention.asInstanceOf[EventMention].trigger)
        if (odinMention.isInstanceOf[CrossSentenceMention]) {
          addMention(odinMention.asInstanceOf[CrossSentenceMention].anchor)
          addMention(odinMention.asInstanceOf[CrossSentenceMention].neighbor)
        }
      })
    }

    surfaceMentions.foreach(addMention)
    mentionBagger.get()
  }

  def findUnderlyingMentions(surfaceMentions: Seq[Mention]): Seq[Mention] = {
    val reachableMentions = findReachableMentions(surfaceMentions)
    val underlyingMentions = reachableMentions.filter { reachableMention =>
      !surfaceMentions.exists { surfaceMention =>
        surfaceMention.eq(reachableMention)
      }
    }

    underlyingMentions
  }

  def hasUnderlyingMentions(surfaceMentions: Seq[Mention]): Boolean = findUnderlyingMentions(surfaceMentions).nonEmpty
}

class EidosTextBoundMention(val odinTextBoundMention: TextBoundMention, canonicalizer: Canonicalizer, ontologyGrounder: MultiOntologyGrounder,
    mentionMapper: MentionMapper)
    extends EidosMention(odinTextBoundMention, canonicalizer, ontologyGrounder, mentionMapper) {

  protected override def canonicalMentions: Seq[Mention] = Seq(odinMention)
}

class EidosEventMention(val odinEventMention: EventMention, canonicalizer: Canonicalizer, ontologyGrounder: MultiOntologyGrounder,
    mentionMapper: MentionMapper)
    extends EidosMention(odinEventMention, canonicalizer, ontologyGrounder, mentionMapper) {

  val odinTrigger = odinEventMention.trigger

  val eidosTrigger = remapOdinMention(odinTrigger, canonicalizer, ontologyGrounder, mentionMapper)

  protected override def canonicalMentions: Seq[Mention] =
      super.canonicalMentions ++ Seq(odinTrigger)
}

class EidosRelationMention(val odinRelationMention: RelationMention, canonicalizer: Canonicalizer, ontologyGrounder: MultiOntologyGrounder,
    mentionMapper: MentionMapper)
    extends EidosMention(odinRelationMention, canonicalizer, ontologyGrounder, mentionMapper) {
}

class EidosCrossSentenceMention(val odinCrossSentenceMention: CrossSentenceMention, canonicalizer: Canonicalizer, ontologyGrounder: MultiOntologyGrounder,
    mentionMapper: MentionMapper)
    extends EidosMention(odinCrossSentenceMention, canonicalizer, ontologyGrounder, mentionMapper) {

  val odinAnchor = odinCrossSentenceMention.anchor

  val eidosAnchor = remapOdinMention(odinAnchor, canonicalizer, ontologyGrounder, mentionMapper)

  val odinNeighbor = odinCrossSentenceMention.neighbor

  val eidosNeighbor = remapOdinMention(odinNeighbor, canonicalizer, ontologyGrounder, mentionMapper)

  protected override def canonicalMentions: Seq[Mention] =
    Seq(odinAnchor, odinNeighbor)
}
