package org.clulab.wm.eidos.mentions

import java.util.IdentityHashMap

import org.clulab.odin.EventMention
import org.clulab.odin.Mention
import org.clulab.odin.RelationMention
import org.clulab.odin.TextBoundMention
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.groundings.Aliases.Groundings
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.EidosAttachment
import org.clulab.wm.eidos.utils.{StopwordManager, StopwordManaging}

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

abstract class EidosMention(val odinMention: Mention, stopwordManaging: StopwordManaging, ontologyGrounder: MultiOntologyGrounder,
    mentionMapper: MentionMapper) /* extends Mention if really needs to */ {
  // This must happen before the remap in case arguments point back to this
  mentionMapper.put(odinMention, this)

  // Accessor method to facilitate cleaner code downstream
  val label = odinMention.label

  // Convenience function for parallel construction
  val odinArguments: Map[String, Seq[Mention]] = odinMention.arguments

  // Access to new and improved Eidos arguments
  val eidosArguments: Map[String, Seq[EidosMention]] = remapOdinArguments(odinArguments, stopwordManaging, ontologyGrounder, mentionMapper)

  val eidosMentionsFromAttachments: Seq[EidosMention] = {
    val attachmentMentions = odinMention.attachments.toSeq.flatMap(_.asInstanceOf[EidosAttachment].attachmentMentions)

    EidosMention.asEidosMentions(attachmentMentions, stopwordManaging, ontologyGrounder, mentionMapper)
  }

  protected def remapOdinArguments(odinArguments: Map[String, Seq[Mention]], stopwordManaging: StopwordManaging, ontologyGrounder: MultiOntologyGrounder,
      mentionMapper: MentionMapper): Map[String, Seq[EidosMention]] = {
    odinArguments.mapValues(odinMentions => EidosMention.asEidosMentions(odinMentions, stopwordManaging, ontologyGrounder, mentionMapper))
  }

  val canonicalName: String // Determined by subclass
  val grounding: Groundings // Determined by subclass, in part because dependent on canonicalName

  // Some way to calculate or store these, possibly in subclass
  def tokenIntervals: Seq[Interval] = Seq(odinMention.tokenInterval)
  def negation: Boolean = ???

  /* Methods for canonicalForms of Mentions */
  protected def canonicalFormSimple(m: Mention): String = {
    def isContentTag(tag: String) = tag.startsWith("NN") || tag.startsWith("VB")
    def removeNER(ner: String) = StopwordManager.STOP_NER.contains(ner)
    val contentLemmas = for {
      (lemma, i) <- m.lemmas.get.zipWithIndex
      tag = m.tags.get(i)
      ner = m.entities.get(i)
      if isContentTag(tag)
      if !stopwordManaging.containsStopword(lemma)
      if !removeNER(ner)
    } yield lemma

    // fixme -- better and cleaner backoff
    if (contentLemmas.isEmpty) {
      m.text
    } else {
      contentLemmas.mkString(" ").trim.replaceAll(" +", " ")
    }
//    println("  * result: " + contentLemmas.mkString(" "))

  }
}

object EidosMention {

  protected def newMentionMapper() = new HashCodeMapper()
//  protected def newMentionMapper() = new IdentityMapper()

  protected def newMentionBagger() = new HashCodeBagger[Mention]()
//  protected def newMentionBagger() = new IdentityBagger[Mention]()

  def newEidosMention(odinMention: Mention, stopwordManaging: StopwordManaging, ontologyGrounder: MultiOntologyGrounder,
      mentionMapper: MentionMapper): EidosMention = {
    odinMention match {
      case mention: TextBoundMention => new EidosTextBoundMention(mention, stopwordManaging, ontologyGrounder, mentionMapper)
      case mention: EventMention => new EidosEventMention(mention, stopwordManaging, ontologyGrounder, mentionMapper)
      case mention: RelationMention => new EidosRelationMention(mention, stopwordManaging, ontologyGrounder, mentionMapper)
      case _ => throw new IllegalArgumentException("Unknown Mention: " + odinMention)
    }
  }

  def asEidosMentions(odinMentions: Seq[Mention], stopwordManaging: StopwordManaging, ontologyGrounder: MultiOntologyGrounder,
      mentionMapper: MentionMapper): Seq[EidosMention] = {
    val eidosMentions = odinMentions.map { odinMention =>
      mentionMapper.getOrElse(odinMention, newEidosMention(odinMention, stopwordManaging, ontologyGrounder, mentionMapper))
    }
    eidosMentions
  }

  def asEidosMentions(odinMentions: Seq[Mention], stopwordManaging: StopwordManaging, ontologyGrounder: MultiOntologyGrounder): Seq[EidosMention] =
      // One could optionally keep this map around
      asEidosMentions(odinMentions, stopwordManaging, ontologyGrounder, newMentionMapper()): Seq[EidosMention]

  def findReachableMentions(surfaceMentions: Seq[Mention]): Seq[Mention] = {
    val mentionBagger = newMentionBagger()

    def addMention(odinMention: Mention): Unit = {
      mentionBagger.putIfNew(odinMention, {
        odinMention.arguments.flatMap(_._2).foreach(addMention)
        // Skipping paths
        odinMention.attachments.asInstanceOf[Set[EidosAttachment]].flatMap(_.attachmentMentions).foreach(addMention)
        if (odinMention.isInstanceOf[EventMention])
          addMention(odinMention.asInstanceOf[EventMention].trigger)
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

class EidosTextBoundMention(val odinTextBoundMention: TextBoundMention, stopwordManaging: StopwordManaging, ontologyGrounder: MultiOntologyGrounder,
    mentionMapper: MentionMapper)
    extends EidosMention(odinTextBoundMention, stopwordManaging, ontologyGrounder, mentionMapper) {

  override val canonicalName: String = canonicalFormSimple(odinMention)
  override val grounding: Map[String, OntologyGrounding] = ontologyGrounder.groundOntology(this)
}

class EidosEventMention(val odinEventMention: EventMention, stopwordManaging: StopwordManaging, ontologyGrounder: MultiOntologyGrounder,
    mentionMapper: MentionMapper)
    extends EidosMention(odinEventMention, stopwordManaging, ontologyGrounder, mentionMapper) {

  val odinTrigger = odinEventMention.trigger

  val eidosTrigger = remapOdinTrigger(odinEventMention.trigger, ontologyGrounder, mentionMapper)

  protected def remapOdinTrigger(odinMention: Mention, ontologyGrounder: MultiOntologyGrounder, mentionMapper: MentionMapper): EidosMention =
      mentionMapper.getOrElse(odinMention, EidosMention.asEidosMentions(Seq(odinMention), stopwordManaging, ontologyGrounder, mentionMapper)(0))

  override val canonicalName = {
    val em = odinEventMention
    val argCanonicalNames = em.arguments.values.flatten.map(arg => (canonicalFormSimple(arg), arg.start)).toSeq
    val argsAndTrigger = argCanonicalNames ++ Seq((canonicalFormSimple(em.trigger), em.trigger.start))
    val sorted = argsAndTrigger.sortBy(_._2)

    sorted.unzip._1.mkString(" ")
  }

  override val grounding: Map[String, OntologyGrounding] = ontologyGrounder.groundOntology(this)
}

class EidosRelationMention(val odinRelationMention: RelationMention, stopwordManaging: StopwordManaging, ontologyGrounder: MultiOntologyGrounder,
    mentionMapper: MentionMapper)
    extends EidosMention(odinRelationMention, stopwordManaging, ontologyGrounder, mentionMapper) {
  
  override val canonicalName = {
    val rm = odinRelationMention
    val argCanonicalNames = rm.arguments.values.flatten.map(arg => (canonicalFormSimple(arg), arg.start)).toSeq
    val sorted = argCanonicalNames.sortBy(_._2)

    sorted.unzip._1.mkString(" ")
  }

  override val grounding: Map[String, OntologyGrounding] = ontologyGrounder.groundOntology(this)
}
