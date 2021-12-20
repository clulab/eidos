package org.clulab.wm.eidos.mentions

import org.clulab.odin._
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.EidosAttachment
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.groundings.grounders.AdjectiveGrounder
import org.clulab.wm.eidos.utils.FoundBy
import org.clulab.wm.eidos.utils.Unordered
import org.clulab.wm.eidos.utils.Unordered.OrderingOrElseBy
import org.clulab.wm.eidoscommon.Canonicalizer
import org.clulab.wm.eidoscommon.EidosParameters
import org.clulab.wm.eidoscommon.utils.IdentityHashMap
import org.clulab.wm.eidoscommon.utils.Logging
import org.clulab.wm.eidoscommon.utils.IdentityHashBag

import scala.collection.mutable

// In order to create this all at once with all OdinMentions that are == being rerouted
// to those being eq(), the mapping needs to be provided and all values calculated upon
// construction.  This implies recursion.  There's no avoiding it.

// This first odinMention should be valid.  It is a key in odinMentionMap and will be one in eidosMentionMap.
abstract class EidosMention(val odinMention: Mention, odinMentionMap: EidosMention.OdinMentionMap,
    eidosMentionMap: EidosMention.EidosMentionMap) {

  // This must happen before the remap in case arguments point back to this
  eidosMentionMap.put(odinMention, this)

  // Convenience function for parallel construction
  val odinArguments: Map[String, Seq[Mention]] = remapOdinArguments(odinMention.arguments, odinMentionMap)

  // Access to new and improved Eidos arguments
  val eidosArguments: Map[String, Seq[EidosMention]] = remapOdinArguments(odinArguments, odinMentionMap, eidosMentionMap)

  // These are filled in by the EidosSystem's default PostProcessor.
  // Default values are used instead of Option to simplify client code.
  var canonicalName: String = ""
  var grounding: OntologyAliases.OntologyGroundingMap = EidosMention.NO_ONTOLOGY_GROUNDINGS
  var deserializedGrounding: OntologyAliases.OntologyGroundingMap = EidosMention.NO_DESERIALIZED_ONTOLOGY_GROUNDINGS
  var classificationOpt: Option[Float] = None

  // Accessor method to facilitate cleaner code downstream
  def label: String = odinMention.label

  // Return any mentions that are involved in the canonical name.  By default, the argument values.
  // This is here to allow subclasses to override it so that the Canonicalizer doesn't have to keep track.
  def canonicalMentions: Seq[Mention] = odinArguments.values.flatten.toSeq

  // You'd think this would just be OdinMention.getNeighbors(odinMention).  However,
  // the neighbors of odinMention may have been replaced by others that are ==.
  def getOdinNeighbors: Seq[Mention] = odinArguments.values.flatten.toSeq

  // Other EidosMentions which can be reached from this.
  def getEidosNeighbors: Seq[EidosMention] = eidosArguments.values.flatten.toSeq

  // Some way to calculate or store these, possibly in subclass
  def tokenIntervals: Seq[Interval] = Seq(odinMention.tokenInterval)
//  def negation: Boolean = ???

  def groundAdjectives(adjectiveGrounder: AdjectiveGrounder): Unit = odinMention
      .attachments
      .collect { case attachment: EidosAttachment => attachment }
      .foreach(_.groundAdjective(adjectiveGrounder))

  protected def remapOdinArguments(odinArguments: Map[String, Seq[Mention]],
      odinMentionMap: EidosMention.OdinMentionMap): Map[String, Seq[Mention]] = {
    odinArguments.map { case (key, odinMentions) =>
      key -> odinMentions.map(odinMentionMap)
    }
  }

  protected def remapOdinArguments(odinArguments: Map[String, Seq[Mention]],
                                   odinMentionMap: EidosMention.OdinMentionMap, eidosMentionMap: EidosMention.EidosMentionMap):
      Map[String, Seq[EidosMention]] = {
    val result = odinArguments.map { case (key, odinMentions) =>
      val mappedValues = EidosMention.asEidosMentions(odinMentions, odinMentionMap, eidosMentionMap)

      key -> mappedValues
    }

    result
  }

  protected def remapOdinMention(odinMention: Mention, odinMentionMap: EidosMention.OdinMentionMap,
                                 eidosMentionMap: EidosMention.EidosMentionMap): EidosMention =
    EidosMention.asEidosMentions(Seq(odinMention), odinMentionMap, eidosMentionMap).head
}

object EidosMention extends Logging {
  val NO_ONTOLOGY_GROUNDINGS = Map.empty[String, OntologyGrounding]
  val NO_DESERIALIZED_ONTOLOGY_GROUNDINGS = Map.empty[String, OntologyGrounding]

  // This maps any Odin Mention onto its canonical one.
  type OdinMentionMap = mutable.Map[Mention, Mention] // should be identity
  // This then maps any canonical one onto the matching EidosMention.
  type EidosMentionMap = mutable.Map[Mention, EidosMention]

  protected def newEidosMention(odinMention: Mention, odinMentionMap: OdinMentionMap, eidosMentionMap: EidosMentionMap): EidosMention = {
    odinMention match {
      case mention: TextBoundMention => new EidosTextBoundMention(mention, odinMentionMap, eidosMentionMap)
      // TODO: Provenance for these mentions probably needs to be improved.
      // These CrossSentenceEventMentions are headed to the same place as the EventMention for now in the jsonld,
      // so they are not distinguished here.  They might be in the future.
      // Right now this is only migration and we're not especially using that right now.
      //case mention: CrossSentenceEventMention => new EidosCrossSentenceEventMention(mention, odinMentionMap, eidosMentionMap)
      case mention: EventMention => new EidosEventMention(mention, odinMentionMap, eidosMentionMap)
      case mention: RelationMention => new EidosRelationMention(mention, odinMentionMap, eidosMentionMap)
      case mention: CrossSentenceMention => new EidosCrossSentenceMention(mention, odinMentionMap, eidosMentionMap)
      case _ => throw new IllegalArgumentException("Unknown Mention: " + odinMention)
    }
  }

  protected def asEidosMentions(odinMentions: Seq[Mention], odinMentionMap: OdinMentionMap, eidosMentionMap: EidosMentionMap): Seq[EidosMention] = {
    val eidosMentions = odinMentions.map { keyOdinMention =>
      val valueOdinMention = odinMentionMap(keyOdinMention)

      eidosMentionMap.getOrElse(valueOdinMention, newEidosMention(valueOdinMention, odinMentionMap, eidosMentionMap))
    }
    eidosMentions
  }

  // This is the main entry point!  These should be the "surface" odinMentions.
  def asEidosMentions(odinMentions: Seq[Mention]): (Seq[EidosMention], Seq[EidosMention]) = {
    val distinctOdinMentions = odinMentions.distinct // This is by == then.
    if (odinMentions.size != distinctOdinMentions.size)
      logger.warn("The Odin mentions are not distinct.")

    val allOdinMentions = OdinMention.findAllByIdentity(odinMentions)
    // Anything that is == to the key will be stored in the values.
    val groupedOdinMentions: Map[Mention, Seq[Mention]] = allOdinMentions.groupBy(odinMention => odinMention)
    // Find the best representative from each group and use that as key.
    val regroupedOdinMentions = groupedOdinMentions.map { case (keyOdinMention, valueOdinMentions) =>
      val newKeyOdinMention = valueOdinMentions.minBy { odinMention =>
        // Use the one with the fewest number of rules and upon tie, the first in alphabetical order.
        (FoundBy.size(odinMention), odinMention.foundBy)
      }
      //if (newKeyOdinMention.foundBy != keyOdinMention.foundBy)
      //  println("It changed!")
      newKeyOdinMention -> valueOdinMentions
    }
    // This will map each odin mention back to the representative mention by identity.
    val odinMentionMap = IdentityHashMap[Mention, Mention]()
    regroupedOdinMentions.foreach { case (keyOdinMention, valueOdinMentions) =>
      valueOdinMentions.foreach { valueOdinMention =>
        odinMentionMap(valueOdinMention) = keyOdinMention
      }
    }
    // Check to make sure there are no concepts that aren't referred to by a relation.
    // If keepStatefulConcepts is true, there may be orphans but otherwise not.
    // This test is fairly expensive and has been run in advance across may documents.
    // A stray orphan would not be catastrophic, so the test is being skipped for now,
    // hasOrphanedConcepts(regroupedOdinMentions, odinMentionMap)

    val eidosMentionMap = IdentityHashMap[Mention, EidosMention]()
    val eidosMentions = asEidosMentions(distinctOdinMentions, odinMentionMap, eidosMentionMap)
    if (groupedOdinMentions.size != eidosMentionMap.size)
      logger.warn("Not all Odin mentions were converted into Eidos mentions.")

    val allEidosMentions = eidosMentionMap.values.toSeq
    (eidosMentions, allEidosMentions)
  }

  def hasOrphanedConcepts(keyMentionMap: Map[Mention, Seq[Mention]], mentionMap: OdinMentionMap): Boolean = {
    val (concepts, relations) = keyMentionMap.keys.partition(_ matches EidosParameters.CONCEPT_LABEL)
    val parentedConcepts = {
      val parentedConcepts = IdentityHashMap[Mention, Boolean]()
      concepts.foreach(concept => parentedConcepts.put(concept, false))
      parentedConcepts
    }

    relations.foreach { relation =>
      val children = OdinMention.getNeighbors(relation)
      val keyChildren = children.map(neighbor => mentionMap(neighbor))
      keyChildren
        .filter(_ matches EidosParameters.CONCEPT_LABEL)
        .foreach(keyConcept => parentedConcepts(keyConcept) = true)
    }

    // Exists would be faster but less informative.  This is for debugging, so choose informative.
    val orphanedConcepts = concepts.filter(concept => !parentedConcepts(concept))
    if (orphanedConcepts.nonEmpty)
      println("There is an orphan!") // Set a breakpoint here.
    orphanedConcepts.nonEmpty
  }

  def findAllByIdentity(surfaceMentions: Seq[EidosMention]): Seq[EidosMention] = {
    // For the EidosMentions, identity should be used because it is faster
    // and the underlying Odin mentions are known to be distinct.
    IdentityHashBag[EidosMention](surfaceMentions, { eidosMention: EidosMention => eidosMention.getEidosNeighbors }).toSeq
  }


  // This is the filtering method for deciding what makes it into the canonical name and what doesn't.
  def canonicalTokensSimple(canonicalizer: Canonicalizer, odinMention: Mention, excludedWords: Set[String]): Seq[String] = {
    canonicalizer.canonicalWordsFromSentence(odinMention.sentenceObj, odinMention.tokenInterval, excludedWords)
  }

  /**
    * To handle mentions that span multiple sentences, we sort the pieces of the mention and then filter each
    * to get the tokens that will make it into the canonicalName.
    */
  def canonicalNameParts(canonicalizer: Canonicalizer, eidosMention: EidosMention, excludedWords: Set[String]): Array[String] = {
    implicit val ordering = Unordered[Mention]
        .orElseBy(_.sentence)
        .orElseBy(_.start)
        .orElseBy(_.end)
        .orElseBy(_.hashCode)

    eidosMention.canonicalMentions.sorted.flatMap(canonicalTokensSimple(canonicalizer, _, excludedWords)).toArray
  }

  def canonicalize(canonicalizer: Canonicalizer, eidosMention: EidosMention, excludedWords: Set[String]): String = canonicalNameParts(canonicalizer, eidosMention, excludedWords).mkString(" ")
}

class EidosTextBoundMention(val odinTextBoundMention: TextBoundMention, odinMentionMap: EidosMention.OdinMentionMap, eidosMentionMap: EidosMention.EidosMentionMap)
    extends EidosMention(odinTextBoundMention, odinMentionMap, eidosMentionMap) {

  override def canonicalMentions: Seq[Mention] = Seq(odinMention)
}

class EidosEventMention(val odinEventMention: EventMention, odinMentionMap: EidosMention.OdinMentionMap, eidosMentionMap: EidosMention.EidosMentionMap)
    extends EidosMention(odinEventMention, odinMentionMap, eidosMentionMap) {

  val odinTrigger: TextBoundMention = odinMentionMap(odinEventMention.trigger).asInstanceOf[TextBoundMention]

  val eidosTrigger: EidosMention = remapOdinMention(odinTrigger, odinMentionMap, eidosMentionMap)

  override def canonicalMentions: Seq[Mention] = super.canonicalMentions ++ Seq(odinTrigger)

  override def getEidosNeighbors: Seq[EidosMention] = super.getEidosNeighbors ++ Seq(eidosTrigger)
}

class EidosCrossSentenceEventMention(val crossSentenceEventMention: CrossSentenceEventMention, odinMentionMap: EidosMention.OdinMentionMap, eidosMentionMap: EidosMention.EidosMentionMap)
    extends EidosEventMention(crossSentenceEventMention, odinMentionMap, eidosMentionMap) {
}

class EidosRelationMention(val odinRelationMention: RelationMention, odinMentionMap: EidosMention.OdinMentionMap, eidosMentionMap: EidosMention.EidosMentionMap)
    extends EidosMention(odinRelationMention, odinMentionMap, eidosMentionMap) {
}

class EidosCrossSentenceMention(val odinCrossSentenceMention: CrossSentenceMention, odinMentionMap: EidosMention.OdinMentionMap, eidosMentionMap: EidosMention.EidosMentionMap)
    extends EidosMention(odinCrossSentenceMention, odinMentionMap, eidosMentionMap) {

  val odinAnchor: Mention = odinMentionMap(odinCrossSentenceMention.anchor)

  val eidosAnchor: EidosMention = remapOdinMention(odinAnchor, odinMentionMap, eidosMentionMap)

  val odinNeighbor: Mention = odinMentionMap(odinCrossSentenceMention.neighbor)

  val eidosNeighbor: EidosMention = remapOdinMention(odinNeighbor, odinMentionMap, eidosMentionMap)

  override def canonicalMentions: Seq[Mention] = Seq(odinAnchor, odinNeighbor)

  override def getEidosNeighbors: Seq[EidosMention] = super.getEidosNeighbors ++ Seq(eidosAnchor, eidosNeighbor)
}
