package org.clulab.wm.eidos.mentions

import java.util.IdentityHashMap // Unfortunately borrowed from Java

import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.odin.EventMention
import org.clulab.odin.Mention
import org.clulab.odin.RelationMention
import org.clulab.odin.TextBoundMention
import org.clulab.wm.eidos.SameAsGrounder
import org.clulab.wm.eidos.SameAsGrounding
import org.clulab.struct.Interval

abstract class EidosMention(val odinMention: Mention, sameAsGrounder: SameAsGrounder,
    mapOfMentions: IdentityHashMap[Mention, EidosMention]) /* extends Mention if really needs to */ {
  // This must happen before the remap in case arguments point back to this
  mapOfMentions.put(odinMention, this)
  
  // Convenience function for parallel construction
  val odinArguments: Map[String, Seq[Mention]] = odinMention.arguments
  
  // Access to new and improved Eidos arguments
  val eidosArguments: Map[String, Seq[EidosMention]] = remapOdinArguments(odinArguments, sameAsGrounder, mapOfMentions)
  
  protected def remapOdinArguments(odinArguments: Map[String, Seq[Mention]], sameAsGrounder: SameAsGrounder,
      mapOfMentions: IdentityHashMap[Mention, EidosMention]): Map[String, Seq[EidosMention]] = {
    odinArguments.mapValues(odinMentions => EidosMention.asEidosMentions(odinMentions, sameAsGrounder, mapOfMentions))
  }
  
  val canonicalName: String // Determined by subclass

  // Some way to calculate or store these, possibly in subclass
  def tokenIntervals: Seq[Interval] = Seq(odinMention.tokenInterval)
  def negation: Boolean = ???

  /* Methods for canonicalForms of Mentions */

  protected def canonicalFormSimple(m: Mention): String = {
    def isContentTag(tag: String) = tag.startsWith("NN") || tag.startsWith("VB")
    
//    println("-> Using canonical form simple on: " + m.text)
    val s = m.document.sentences(m.sentence)
    val tags = s.tags.get.slice(m.start, m.end)
    val lemmas = s.lemmas.get.slice(m.start, m.end)
    val contentLemmas = for {
      (tag, lemma) <- tags.zip(lemmas)
      if isContentTag(tag)
    } yield lemma

//    println("  * result: " + contentLemmas.mkString(" "))
    contentLemmas.mkString(" ")
  }
}

object EidosMention {
  
  def newEidosMention(odinMention: Mention, sameAsGrounder: SameAsGrounder,
        mapOfMentions: IdentityHashMap[Mention, EidosMention]): EidosMention = {
    odinMention match {
      case mention: TextBoundMention => new EidosTextBoundMention(mention, sameAsGrounder, mapOfMentions)
      case mention: EventMention => new EidosEventMention(mention, sameAsGrounder, mapOfMentions)
      case mention: RelationMention => new EidosRelationMention(mention, sameAsGrounder, mapOfMentions)
      case _ => throw new IllegalArgumentException("Unknown Mention: " + odinMention)
    }
  }
  
  def asEidosMentions(odinMentions: Seq[Mention], sameAsGrounder: SameAsGrounder,
        mapOfMentions: IdentityHashMap[Mention, EidosMention]): Seq[EidosMention] = {
    val eidosMentions = odinMentions.map { odinMention =>
      if (mapOfMentions.containsKey(odinMention))
        mapOfMentions.get(odinMention)
      else
        EidosMention.newEidosMention(odinMention, sameAsGrounder, mapOfMentions)
    }
    eidosMentions
  }
  
  def asEidosMentions(odinMentions: Seq[Mention], sameAsGrounder: SameAsGrounder): Seq[EidosMention] =
      // One could optionally keep this map around
      asEidosMentions(odinMentions, sameAsGrounder, new IdentityHashMap[Mention, EidosMention]()): Seq[EidosMention]
}

class EidosTextBoundMention(val odinTextBoundMention: TextBoundMention, sameAsGrounder: SameAsGrounder,
      mapOfMentions: IdentityHashMap[Mention, EidosMention])
      extends EidosMention(odinTextBoundMention, sameAsGrounder, mapOfMentions) {
  
  override val canonicalName: String = canonicalFormSimple(odinMention)

  def grounding: SameAsGrounding = sameAsGrounder.ground(canonicalName)
}

class EidosEventMention(val odinEventMention: EventMention, sameAsGrounder: SameAsGrounder,
    mapOfMentions: IdentityHashMap[Mention, EidosMention])
    extends EidosMention(odinEventMention, sameAsGrounder, mapOfMentions) {
  
  val odinTrigger = odinEventMention.trigger
  
  val eidosTrigger = remapOdinTrigger(odinEventMention.trigger, sameAsGrounder, mapOfMentions)
  
  protected def remapOdinTrigger(odinMention: Mention, sameAsGrounder: SameAsGrounder, mapOfMentions: IdentityHashMap[Mention, EidosMention]): EidosMention =
    if (mapOfMentions.containsKey(odinMention)) mapOfMentions.get(odinMention)
    else EidosMention.asEidosMentions(Seq(odinMention), sameAsGrounder, mapOfMentions)(0)

  override val canonicalName = {
    val em = odinEventMention
    val argCanonicalNames = em.arguments.values.flatten.map(arg => (canonicalFormSimple(arg), arg.start)).toSeq
    val argsAndTrigger = argCanonicalNames ++ Seq((canonicalFormSimple(em.trigger), em.trigger.start))
    val sorted = argsAndTrigger.sortBy(_._2)

    sorted.unzip._1.mkString(" ")
  }
}

class EidosRelationMention(val odinRelationMention: RelationMention, sameAsGrounder: SameAsGrounder,
    mapOfMentions: IdentityHashMap[Mention, EidosMention])
    extends EidosMention(odinRelationMention, sameAsGrounder, mapOfMentions) {
  
  override val canonicalName = {
    val rm = odinRelationMention
    val argCanonicalNames = rm.arguments.values.flatten.map(arg => (canonicalFormSimple(arg), arg.start)).toSeq
    val sorted = argCanonicalNames.sortBy(_._2)

    sorted.unzip._1.mkString(" ")
  }
}
