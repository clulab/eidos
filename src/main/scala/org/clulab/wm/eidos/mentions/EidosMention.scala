package org.clulab.wm.eidos.mentions

import java.util.IdentityHashMap // Unfortunately borrowed from Java

import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.odin.EventMention
import org.clulab.odin.Mention
import org.clulab.odin.RelationMention
import org.clulab.odin.TextBoundMention
import org.clulab.struct.Interval

abstract class EidosMention(
                 val odinMention: Mention,
                 mapOfMentions: IdentityHashMap[Mention, EidosMention],
                 w2v: Word2Vec,
                 ontology: Map[String, Seq[Double]],
                 k: Int = 10) /* extends Mention if really needs to */ {
  // This must happen before the remap in case arguments point back to this
  mapOfMentions.put(odinMention, this)

  // Convenience function for parallel construction
  val odinArguments: Map[String, Seq[Mention]] = odinMention.arguments
  
  // Access to new and improved Eidos arguments
  val eidosArguments: Map[String, Seq[EidosMention]] = remapOdinArguments(odinArguments, mapOfMentions, w2v, ontology, k)
  
  protected def remapOdinArguments(
                  odinArguments: Map[String, Seq[Mention]],
                  mapOfMentions: IdentityHashMap[Mention, EidosMention],
                  w2v: Word2Vec,
                  ontology: Map[String, Seq[Double]],
                  k: Int = 10): Map[String, Seq[EidosMention]] =
      odinArguments.mapValues(odinMentions => EidosMention.asEidosMentions(odinMentions, mapOfMentions, w2v, ontology, k))

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
  
  def newEidosMention(
        odinMention: Mention,
        mapOfMentions: IdentityHashMap[Mention, EidosMention],
        w2v: Word2Vec,
        ontology: Map[String, Seq[Double]],
        k: Int = 10): EidosMention =
      odinMention match {
        case mention: TextBoundMention => new EidosTextBoundMention(mention, mapOfMentions,  w2v, ontology, k)
        case mention: EventMention => new EidosEventMention(mention, mapOfMentions, w2v, ontology, k)
        case mention: RelationMention => new EidosRelationMention(mention, mapOfMentions, w2v, ontology, k)
        case _ => throw new IllegalArgumentException("Unknown Mention: " + odinMention)
      }
  
  def asEidosMentions(
        odinMentions: Seq[Mention],
        mapOfMentions: IdentityHashMap[Mention, EidosMention],
        w2v: Word2Vec,
        ontology: Map[String, Seq[Double]],
        k: Int): Seq[EidosMention] = {
    val eidosMentions = odinMentions.map { odinMention =>
      if (mapOfMentions.containsKey(odinMention))
        mapOfMentions.get(odinMention)
      else
        EidosMention.newEidosMention(odinMention, mapOfMentions, w2v, ontology, k)
    }
    eidosMentions
  }
  
  def asEidosMentions(odinMentions: Seq[Mention],
                      w2v: Word2Vec,
                      ontology: Map[String, Seq[Double]],
                      k: Int): Seq[EidosMention] =
      // One could optionally keep this map around
      asEidosMentions(odinMentions, new IdentityHashMap[Mention, EidosMention](), w2v, ontology, k): Seq[EidosMention]
}

class EidosTextBoundMention(
    val odinTextBoundMention: TextBoundMention,
    mapOfMentions: IdentityHashMap[Mention, EidosMention],
    w2v: Word2Vec,
    ontology: Map[String, Seq[Double]],
    k: Int = 10
  ) extends EidosMention(odinTextBoundMention, mapOfMentions, w2v, ontology, k) {
  
  override val canonicalName: String = canonicalFormSimple(odinMention)

  // Some way to calculate or store this, possibly in superclass
  def grounding: Seq[(String, Double)] = {
    // Make vector for canonicalName
    val nodeEmbedding = w2v.makeCompositeVector(canonicalName.split(" +"))
    // Calc dot prods
    val similarities = ontology.toSeq.map(concept => (concept._1, Word2Vec.dotProduct(concept._2.toArray, nodeEmbedding)))
    // sort and return top k
    similarities.sortBy(- _._2).slice(0,k)
  }
}

class EidosEventMention(val odinEventMention: EventMention, mapOfMentions: IdentityHashMap[Mention, EidosMention],  w2v: Word2Vec,
                        ontology: Map[String, Seq[Double]],
                        k: Int = 10)
    extends EidosMention(odinEventMention, mapOfMentions, w2v, ontology, k) {
  
  override val canonicalName = {
    val em = odinEventMention
    val argCanonicalNames = em.arguments.values.flatten.map(arg => (canonicalFormSimple(arg), arg.start)).toSeq
    val argsAndTrigger = argCanonicalNames ++ Seq((canonicalFormSimple(em.trigger), em.trigger.start))
    val sorted = argsAndTrigger.sortBy(_._2)

    sorted.unzip._1.mkString(" ")
  }
}

class EidosRelationMention(val odinRelationMention: RelationMention, mapOfMentions: IdentityHashMap[Mention, EidosMention],  w2v: Word2Vec,
                           ontology: Map[String, Seq[Double]],
                           k: Int = 10)
    extends EidosMention(odinRelationMention, mapOfMentions, w2v, ontology, k) {
  
  override val canonicalName = {
    val rm = odinRelationMention
    val argCanonicalNames = rm.arguments.values.flatten.map(arg => (canonicalFormSimple(arg), arg.start)).toSeq
    val sorted = argCanonicalNames.sortBy(_._2)

    sorted.unzip._1.mkString(" ")
  }
}
