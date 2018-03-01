package org.clulab.wm.eidos.mentions

import java.util.IdentityHashMap  // Unfortunately borrowed from Java

import org.clulab.odin.EventMention
import org.clulab.odin.Mention
import org.clulab.odin.RelationMention
import org.clulab.odin.TextBoundMention
import org.clulab.struct.Interval

abstract class EidosMention(val odinMention: Mention, mapOfMentions: IdentityHashMap[Mention, EidosMention]) /* extends Mention if really needs to */ {
  // This must happen before the remap in case arguments point back to this
  mapOfMentions.put(odinMention, this)

  // Convenience function for parallel construction
  val odinArguments: Map[String, Seq[Mention]] = odinMention.arguments
  
  // Access to new and improved Eidos arguments
  val eidosArguments: Map[String, Seq[EidosMention]] = remapOdinArguments(odinArguments, mapOfMentions)
  
  protected def remapOdinArguments(odinArguments: Map[String, Seq[Mention]], mapOfMentions: IdentityHashMap[Mention, EidosMention]): Map[String, Seq[EidosMention]] =
      odinArguments.mapValues(odinMentions => EidosMention.asEidosMentions(odinMentions, mapOfMentions))

  val canonicalName: String // Determined by subclass

  // Some way to calculate or store these, possibly in subclass
  def tokenIntervals: Seq[Interval] = ???
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
  
  def newEidosMention(odinMention: Mention, mapOfMentions: IdentityHashMap[Mention, EidosMention]): EidosMention =
      odinMention match {
        case mention: TextBoundMention => new EidosTextBoundMention(mention, mapOfMentions)
        case mention: EventMention => new EidosEventMention(mention, mapOfMentions)
        case mention: RelationMention => new EidosRelationMention(mention, mapOfMentions)
        case _ => throw new IllegalArgumentException("Unknown Mention: " + odinMention)
      }
  
  def asEidosMentions(odinMentions: Seq[Mention], mapOfMentions: IdentityHashMap[Mention, EidosMention]): Seq[EidosMention] = {
    val eidosMentions = odinMentions.map { odinMention =>
      if (mapOfMentions.containsKey(odinMention))
        mapOfMentions.get(odinMention)
      else
        EidosMention.newEidosMention(odinMention, mapOfMentions)
    }
    eidosMentions
  }
  
  def asEidosMentions(odinMentions: Seq[Mention]): Seq[EidosMention] =
      // One could optionally keep this map around
      asEidosMentions(odinMentions, new IdentityHashMap[Mention, EidosMention]())
}

class EidosTextBoundMention(val odinTextBoundMention: TextBoundMention, mapOfMentions: IdentityHashMap[Mention, EidosMention])
    extends EidosMention(odinTextBoundMention, mapOfMentions) {
  
  override val canonicalName: String = canonicalFormSimple(odinMention)

  // Some way to calculate or store this, possibly in superclass
  def grounding: Unit = ???
}

class EidosEventMention(val odinEventMention: EventMention, mapOfMentions: IdentityHashMap[Mention, EidosMention])
    extends EidosMention(odinEventMention, mapOfMentions) {
  
  override val canonicalName = {
    val em = odinEventMention
//    println("-> Using canonical form args on: " + em.text)

    val argCanonicalNames = em.arguments.values.flatten.map(arg => (canonicalFormSimple(arg), arg.start)).toSeq
    val argsAndTrigger = argCanonicalNames ++ Seq((canonicalFormSimple(em.trigger), em.trigger.start))
    val sorted = argsAndTrigger.sortBy(_._2)

//    println("  * result: " + sorted.unzip._1.mkString(" "))
    sorted.unzip._1.mkString(" ")
  }
}

class EidosRelationMention(val odinRelationMention: RelationMention, mapOfMentions: IdentityHashMap[Mention, EidosMention])
    extends EidosMention(odinRelationMention, mapOfMentions) {
  
  override val canonicalName = "" // TODO: What would this look like?
}
