package org.clulab.wm.eidos.mentions

import java.util.IdentityHashMap

import org.clulab.odin.EventMention
import org.clulab.odin.Mention
import org.clulab.odin.RelationMention
import org.clulab.odin.TextBoundMention
import org.clulab.wm.eidos.groundings.{EidosOntologyGrounder, OntologyGrounder, OntologyGrounding}
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.EidosAttachment
import org.clulab.wm.eidos.utils.{StopwordManager, StopwordManaging}

abstract class EidosMention(val odinMention: Mention, stopwordManaging: StopwordManaging, sameAsGrounder: OntologyGrounder,
    mapOfMentions: IdentityHashMap[Mention, EidosMention]) /* extends Mention if really needs to */ {
  // This must happen before the remap in case arguments point back to this
  mapOfMentions.put(odinMention, this)
  
  // Convenience function for parallel construction
  val odinArguments: Map[String, Seq[Mention]] = odinMention.arguments
  
  // Access to new and improved Eidos arguments
  val eidosArguments: Map[String, Seq[EidosMention]] = remapOdinArguments(odinArguments, stopwordManaging, sameAsGrounder, mapOfMentions)

  val eidosMentionsFromAttachments: Seq[EidosMention] = {
    val attachmentMentions = odinMention.attachments.toSeq.flatMap(_.asInstanceOf[EidosAttachment].attachmentMentions)

    EidosMention.asEidosMentions(attachmentMentions, stopwordManaging, sameAsGrounder, mapOfMentions)
  }

  protected def remapOdinArguments(odinArguments: Map[String, Seq[Mention]], stopwordManaging: StopwordManaging, sameAsGrounder: OntologyGrounder,
      mapOfMentions: IdentityHashMap[Mention, EidosMention]): Map[String, Seq[EidosMention]] = {
    odinArguments.mapValues(odinMentions => EidosMention.asEidosMentions(odinMentions, stopwordManaging, sameAsGrounder, mapOfMentions))
  }

  val canonicalName: String // Determined by subclass
  val grounding: OntologyGrounding // Determined by subclass, in part because dependent on canonicalName

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
  
  def newEidosMention(odinMention: Mention, stopwordManaging: StopwordManaging, sameAsGrounder: OntologyGrounder,
        mapOfMentions: IdentityHashMap[Mention, EidosMention]): EidosMention = {
    odinMention match {
      case mention: TextBoundMention => new EidosTextBoundMention(mention, stopwordManaging, sameAsGrounder, mapOfMentions)
      case mention: EventMention => new EidosEventMention(mention, stopwordManaging, sameAsGrounder, mapOfMentions)
      case mention: RelationMention => new EidosRelationMention(mention, stopwordManaging, sameAsGrounder, mapOfMentions)
      case _ => throw new IllegalArgumentException("Unknown Mention: " + odinMention)
    }
  }
  
  def asEidosMentions(odinMentions: Seq[Mention], stopwordManaging: StopwordManaging, sameAsGrounder: OntologyGrounder,
        mapOfMentions: IdentityHashMap[Mention, EidosMention]): Seq[EidosMention] = {
    val eidosMentions = odinMentions.map { odinMention =>
      if (mapOfMentions.containsKey(odinMention))
        mapOfMentions.get(odinMention)
      else
        newEidosMention(odinMention, stopwordManaging, sameAsGrounder, mapOfMentions)
    }
    eidosMentions
  }
  
  def asEidosMentions(odinMentions: Seq[Mention], stopwordManaging: StopwordManaging, sameAsGrounder: OntologyGrounder): Seq[EidosMention] =
      // One could optionally keep this map around
      asEidosMentions(odinMentions, stopwordManaging, sameAsGrounder, new IdentityHashMap[Mention, EidosMention]()): Seq[EidosMention]
}

class EidosTextBoundMention(val odinTextBoundMention: TextBoundMention, stopwordManaging: StopwordManaging, sameAsGrounder: OntologyGrounder,
      mapOfMentions: IdentityHashMap[Mention, EidosMention])
      extends EidosMention(odinTextBoundMention, stopwordManaging, sameAsGrounder, mapOfMentions) {
  
  override val canonicalName: String = canonicalFormSimple(odinMention)
  override val grounding: OntologyGrounding = sameAsGrounder.groundOntology(this)
}

class EidosEventMention(val odinEventMention: EventMention, stopwordManaging: StopwordManaging, sameAsGrounder: OntologyGrounder,
    mapOfMentions: IdentityHashMap[Mention, EidosMention])
    extends EidosMention(odinEventMention, stopwordManaging, sameAsGrounder, mapOfMentions) {
  
  val odinTrigger = odinEventMention.trigger
  
  val eidosTrigger = remapOdinTrigger(odinEventMention.trigger, sameAsGrounder, mapOfMentions)
  
  protected def remapOdinTrigger(odinMention: Mention, sameAsGrounder: OntologyGrounder, mapOfMentions: IdentityHashMap[Mention, EidosMention]): EidosMention =
    if (mapOfMentions.containsKey(odinMention)) mapOfMentions.get(odinMention)
    else EidosMention.asEidosMentions(Seq(odinMention), stopwordManaging, sameAsGrounder, mapOfMentions)(0)

  override val canonicalName = {
    val em = odinEventMention
    val argCanonicalNames = em.arguments.values.flatten.map(arg => (canonicalFormSimple(arg), arg.start)).toSeq
    val argsAndTrigger = argCanonicalNames ++ Seq((canonicalFormSimple(em.trigger), em.trigger.start))
    val sorted = argsAndTrigger.sortBy(_._2)

    sorted.unzip._1.mkString(" ")
  }

  override val grounding: OntologyGrounding = sameAsGrounder.groundOntology(this)
}

class EidosRelationMention(val odinRelationMention: RelationMention, stopwordManaging: StopwordManaging, sameAsGrounder: OntologyGrounder,
    mapOfMentions: IdentityHashMap[Mention, EidosMention])
    extends EidosMention(odinRelationMention, stopwordManaging, sameAsGrounder, mapOfMentions) {
  
  override val canonicalName = {
    val rm = odinRelationMention
    val argCanonicalNames = rm.arguments.values.flatten.map(arg => (canonicalFormSimple(arg), arg.start)).toSeq
    val sorted = argCanonicalNames.sortBy(_._2)

    sorted.unzip._1.mkString(" ")
  }

  override val grounding: OntologyGrounding = sameAsGrounder.groundOntology(this)
}
