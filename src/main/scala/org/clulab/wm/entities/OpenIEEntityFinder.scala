package org.clulab.wm.entities

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin.{ExtractorEngine, Mention, State, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.struct.Interval
import org.clulab.wm.wmutils.FileUtils.readRules

import scala.annotation.tailrec

class OpenIEEntityFinder(
  entityEngine: ExtractorEngine,
  avoidEngine: ExtractorEngine,
  maxHops: Int
 ) extends RuleBasedEntityFinder(entityEngine: ExtractorEngine, avoidEngine: ExtractorEngine, maxHops: Int) {

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
    val baseEntities = entityEngine.extractFrom(doc, stateFromAvoid).filter{ entity => ! stateFromAvoid.contains(entity) }


    val expandedEntities: Seq[Mention] = baseEntities.map(entity => expand(entity, maxHops, stateFromAvoid))

    // split entities on likely coordinations
    val splitEntities = (baseEntities ++ expandedEntities).flatMap(splitCoordinatedEntities)


    // remove entity duplicates introduced by splitting expanded
    val distinctEntities = splitEntities.distinct
    // trim unwanted POS from entity edges
    val trimmedEntities = distinctEntities.map(trimEntityEdges)


    // if there are no avoid mentions, no need to filter
    val res = if (avoid.isEmpty) {
      trimmedEntities
    } else {
      val avoidLabel = avoid.head.labels.last
      trimmedEntities.filter{ m => stateFromAvoid.mentionsFor(m.sentence, m.tokenInterval, avoidLabel).isEmpty }
    }


    res
  }

  /**
    * Expands an entity up to the specified number of hops along valid grammatical relations.
    */
  def expand(entity: Mention, maxHops: Int, stateFromAvoid: State): Mention = {
    val interval = traverseOutgoingLocal(entity, maxHops, stateFromAvoid)
    new TextBoundMention(entity.labels, interval, entity.sentence, entity.document, entity.keep, entity.foundBy)
  }


  /** Used by expand to selectively traverse the provided syntactic dependency graph **/
  @tailrec
  private def traverseOutgoingLocal(
                                tokens: Set[Int],
                                newTokens: Set[Int],
                                outgoingRelations: Array[Array[(Int, String)]],
                                incomingRelations: Array[Array[(Int, String)]],
                                remainingHops: Int,
                                sent: Int,
                                stateFromAvoid: State
                              ): Interval = {
    if (remainingHops == 0) {
      val allTokens = tokens ++ newTokens
      Interval(allTokens.min, allTokens.max + 1)
    } else {
      val newNewTokens = for{
        tok <- newTokens
        if outgoingRelations.nonEmpty && tok < outgoingRelations.length
        (nextTok, dep) <- outgoingRelations(tok)
        if isValidOutgoingDependency(dep)
        if stateFromAvoid.mentionsFor(sent, nextTok).isEmpty
        if hasValidIncomingDependencies(nextTok, incomingRelations)
      } yield nextTok
      traverseOutgoingLocal(tokens ++ newTokens, newNewTokens, outgoingRelations, incomingRelations, remainingHops - 1, sent, stateFromAvoid)
    }
  }
  private def traverseOutgoingLocal(m: Mention, numHops: Int, stateFromAvoid: State): Interval = {
    val outgoing = outgoingEdges(m.sentenceObj)
    val incoming = incomingEdges(m.sentenceObj)
    traverseOutgoingLocal(Set.empty, m.tokenInterval.toSet, outgoingRelations = outgoing, incomingRelations = incoming, numHops, m.sentence, stateFromAvoid)
  }

  // regexes describing valid outgoing dependencies
  override val VALID_OUTGOING = Set[scala.util.matching.Regex](
    "^amod$".r, "^advmod$".r,
    "^dobj$".r,
    "^compound".r, // replaces nn
    "^name".r, // this is equivalent to compound when NPs are tagged as named entities, otherwise unpopulated
    // ex.  "isotonic fluids may reduce the risk" -> "isotonic fluids may reduce the risk associated with X."
    "^acl".r, // replaces vmod
    // Changed from processors......
    "^nmod".r, // replaces prep_
//    "case".r
  )

  // Todo: currently does not work for cross-sentence mentions, add functionality
  /**
    * Trims found entities of leading or trailing unwanted tokens.  Currently, we define "unwanted" as being POS tagged
    * with one of the tags in INVALID_EDGE_TAGS.
    * @param entity
    * @return TextBoundMention with valid interval
    */
  def trimEntityEdges(entity: Mention): Mention = {
    // println(s"trying to trim entity: ${entity.text}")
    // Check starting tag, get the location of first valid tag
    val tags = entity.document.sentences(entity.sentence).tags.get
    val startToken = entity.tokenInterval.start
    val startTag = tags(startToken)
    val firstValidStart = if (validEdgeTag(startTag)) startToken else firstValid(tags, startToken)

    // Check ending tag, get the location of last valid tag
    val endToken = entity.tokenInterval.end - 1  // subtracting 1 bc interval is exclusive
    val endTag = tags(endToken)
    val lastValidEnd = if (validEdgeTag(endTag)) endToken else lastValid(tags, endToken)


    if (firstValidStart == startToken && lastValidEnd == endToken) {
      // No trimming needed because both first and last were valid
      entity
    } else if (firstValidStart > lastValidEnd) {
      // If you trimmed everything...
      entity
    }
    else {
      // Return a new entity with the trimmed token interval
      // println(s"firstValidStart = $firstValidStart, lastValidEnd = $lastValidEnd")
      val interval = Interval(firstValidStart, lastValidEnd + 1)
      new TextBoundMention(entity.labels, interval, entity.sentence, entity.document, entity.keep, entity.foundBy)
    }
  }

  // Find the first valid token in the mention's token interval
  def firstValid(tags: Seq[String], mentionStart: Int): Int = {
    // As indexWhere returns -1 in the event it doesn't find any, here we add the max to default to the first token
    math.max(tags.indexWhere(tag => validEdgeTag(tag), from = mentionStart), 0)
  }

  // Find the last valid token in the mention's token interval
  // mentionEnd is inclusive
  def lastValid(tags: Seq[String], mentionEnd: Int): Int = {
    // As indexWhere returns -1 in the event it doesn't find any, here we add the max to default to the first token
    // Note: end is inclusive
    math.max(tags.lastIndexWhere(tag => validEdgeTag(tag), end = mentionEnd), 0)
  }

  def validEdgeTag(tag: String): Boolean = ! INVALID_EDGE_TAGS.exists(pattern => pattern.findFirstIn(tag).nonEmpty)

  // Set of tags that we don't want to begin or end an entity
  val INVALID_EDGE_TAGS = Set[scala.util.matching.Regex](
    "^IN".r,
    "^TO".r,
    "^DT".r
  )

}

object OpenIEEntityFinder extends LazyLogging {

  val DEFAULT_MAX_LENGTH = 10 // maximum length (in tokens) for an entity
  def apply(maxHops: Int, maxLength: Int = DEFAULT_MAX_LENGTH): OpenIEEntityFinder = {
    val entityRules = ResourceUtils.readResource("org/clulab/wm/grammars/entities/grammar/entities.yml")
    val avoidRules = readRules("/org/clulab/wm/grammars/avoidLocal.yml")

    val avoidEngine = ExtractorEngine(avoidRules)
    val entityEngine = ExtractorEngine(entityRules)
    new OpenIEEntityFinder(avoidEngine = avoidEngine, entityEngine = entityEngine, maxHops = maxHops)
  }


}
