package org.clulab.wm.eidos.entities

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin.{ExtractorEngine, Mention, State, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.struct.Interval
import org.clulab.wm.eidos.groundings.EidosOntologyGrounder
import org.clulab.wm.eidos.utils.{FileUtils, StopwordManager}

import scala.annotation.tailrec

class EidosEntityFinder(entityEngine: ExtractorEngine, avoidEngine: ExtractorEngine, maxHops: Int)
    extends RuleBasedEntityFinder(entityEngine: ExtractorEngine, avoidEngine: ExtractorEngine, maxHops: Int) {

  /**
    * Task-specific implementation of extract to find entities in documents.  Additions include (TODO) handling of
    * domain Params, and (TODO) converting to Mentions with Modifications
    * @param doc: Document
    * @return Seq[Mention] of entities
    */
  override def extract(doc: Document): Seq[Mention] = {
    // avoid refs, etc.
    val avoid = avoidEngine.extractFrom(doc)
    val stateFromAvoid = State(avoid)
    // extract the base entities
    val baseEntities = entityEngine.extractFrom(doc, stateFromAvoid).filter{ entity => ! stateFromAvoid.contains(entity) }
    // make sure that all are valid (i.e., contain a noun or would have contained a noun except for trigger avoidance)
    val validBaseEntities = baseEntities //fixme .filter(isValidBaseEntity)
    // expand the entities
//    val expandedEntities: Seq[Mention] = validBaseEntities.map(entity => expand(entity, maxHops, stateFromAvoid))
//    // split entities on likely coordinations
//    val splitEntities = (validBaseEntities ++ expandedEntities).flatMap(splitCoordinatedEntities)
    val splitEntities = validBaseEntities.flatMap(EntityHelper.splitCoordinatedEntities)
    // remove entity duplicates introduced by splitting expanded
    val distinctEntities = splitEntities.distinct
    // trim unwanted POS from entity edges
    val trimmedEntities = distinctEntities.map(EntityHelper.trimEntityEdges)
    // if there are no avoid mentions, no need to filter
    val res = if (avoid.isEmpty) {
      trimmedEntities
    } else {
      val avoidLabel = avoid.head.labels.last
      trimmedEntities.filter{ m => stateFromAvoid.mentionsFor(m.sentence, m.tokenInterval, avoidLabel).isEmpty }
    }

//    println(s"AVOID  -- \n\t${avoid.map(m => m.text + "__" + m.foundBy).mkString("\n\t")}")
//    println(s"Base-entities  -- \n\t${baseEntities.map(m => m.text).mkString("\n\t")}")
////    println(s"Expanded-entities  -- \n\t${expandedEntities.map(m => m.text).mkString("\n\t")}")
//    println(s"distinct-Entities -- \n\t${distinctEntities.map(m => m.text).mkString("\n\t")}")
//    println(s"trimmed-Entities -- \n\t${trimmedEntities.map(m => m.text).mkString("\n\t")}")
//    println(s"Entities finally returned -- \n\t${res.map(m => m.text).mkString("\n\t")}")
    res
  }



  /**
    * Determines whether or not an entity is a valid base entity. We want to disallow JJ-only entities except
    * when they are a result of the head noun being a trigger (i.e. being avoided)
    */
  def isValidBaseEntity(entity: Mention): Boolean = {
    // Helper method for determining if the next word after the entity is a noun
    def nextTagNN(entity: Mention): Boolean = {
      val tags = entity.sentenceObj.tags.get

      entity.end < tags.length &&
          tags(entity.end).startsWith("NN")
    }

    def containsValidNounVerb(entity: Mention): Boolean = {
      //val lemmas = entity.lemmas.get
      val tags = entity.tags.get
      val entities = entity.entities.get

      // Make sure there is a noun that isn't a named entity.  We can also check for stop words with some re-architecting...
      tags.indices.exists { i =>
        (tags(i).startsWith("NN") || tags(i).startsWith("VB")) &&
        !StopwordManager.STOP_NER.contains(entities(i))
      }
    }
    // If there's a non-named entity noun in the entity, it's valid
    containsValidNounVerb(entity) ||
    // Otherwise, if the entity ends with an adjective and the next word is a noun (which was excluded because ]
    // it's needed as a trigger downstream), it's valid (ex: 'economic declines')
    entity.tags.get.last.startsWith("JJ") && nextTagNN(entity)
    // Otherwise, it's not valid
  }


  /**
    * Expands an entity up to the specified number of hops along valid grammatical relations.
    */
//  def expand(entity: Mention, maxHops: Int, stateFromAvoid: State): Mention = {
//    val interval = traverseOutgoingLocal(entity, maxHops, stateFromAvoid)
//    new TextBoundMention(entity.labels, interval, entity.sentence, entity.document, entity.keep, entity.foundBy)
//  }
//
//
//  /** Used by expand to selectively traverse the provided syntactic dependency graph **/
//  @tailrec
//  private def traverseOutgoingLocal(
//                                tokens: Set[Int],
//                                newTokens: Set[Int],
//                                outgoingRelations: Array[Array[(Int, String)]],
//                                incomingRelations: Array[Array[(Int, String)]],
//                                remainingHops: Int,
//                                sent: Int,
//                                stateFromAvoid: State
//                              ): Interval = {
//    if (remainingHops == 0) {
//      val allTokens = tokens ++ newTokens
//      Interval(allTokens.min, allTokens.max + 1)
//    } else {
//      val newNewTokens = for{
//        tok <- newTokens
//        if outgoingRelations.nonEmpty && tok < outgoingRelations.length
//        (nextTok, dep) <- outgoingRelations(tok)
//        if isValidOutgoingDependency(dep)
//        if stateFromAvoid.mentionsFor(sent, nextTok).isEmpty
//        if hasValidIncomingDependencies(nextTok, incomingRelations)
//      } yield nextTok
//      traverseOutgoingLocal(tokens ++ newTokens, newNewTokens, outgoingRelations, incomingRelations, remainingHops - 1, sent, stateFromAvoid)
//    }
//  }
//  private def traverseOutgoingLocal(m: Mention, numHops: Int, stateFromAvoid: State): Interval = {
//    val outgoing = outgoingEdges(m.sentenceObj)
//    val incoming = incomingEdges(m.sentenceObj)
//    traverseOutgoingLocal(Set.empty, m.tokenInterval.toSet, outgoingRelations = outgoing, incomingRelations = incoming, numHops, m.sentence, stateFromAvoid)
//  }


  // Todo: currently does not work for cross-sentence mentions, add functionality

  // Debug Methods
  private def debugDisplay(s: String, mentions: Seq[Mention]): Unit = {
    println(s"$s:")
    mentions.foreach(m => println(s"\t${m.text}"))
  }
}

object EidosEntityFinder extends LazyLogging {
  val DEFAULT_MAX_LENGTH = RuleBasedEntityFinder.DEFAULT_MAX_LENGTH // maximum length (in tokens) for an entity
  
  def apply(entityRulesPath: String, avoidRulesPath: String, maxHops: Int, maxLength: Int = DEFAULT_MAX_LENGTH): EidosEntityFinder = {
    val entityRules = FileUtils.getTextFromResource(entityRulesPath)
    val entityEngine = ExtractorEngine(entityRules)

    val avoidRules = FileUtils.getTextFromResource(avoidRulesPath)
    val avoidEngine = ExtractorEngine(avoidRules)

    new EidosEntityFinder(entityEngine = entityEngine, avoidEngine = avoidEngine, maxHops = maxHops)
  }
}
