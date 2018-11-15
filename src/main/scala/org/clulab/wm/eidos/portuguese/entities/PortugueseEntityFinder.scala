package org.clulab.wm.eidos.portuguese.entities

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin.{ExtractorEngine, Mention, State, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.struct.Interval
import org.clulab.wm.eidos.EidosActions
import org.clulab.wm.eidos.entities.{EntityHelper, RuleBasedEntityFinder}
import org.clulab.wm.eidos.portuguese.actions.PortugueseActions
import org.clulab.wm.eidos.utils.{DisplayUtils, FileUtils, StopwordManager}


class PortugueseEntityFinder(entityEngine: ExtractorEngine, avoidEngine: ExtractorEngine, maxHops: Int)
  extends RuleBasedEntityFinder(entityEngine: ExtractorEngine, avoidEngine: ExtractorEngine, maxHops: Int) {

  /**
    * Task-specific implementation of extract to find entities in documents.
    * @param doc: Document
    * @return Seq[Mention] of entities
    */
  override def extract(doc: Document): Seq[Mention] = {
    // avoid refs, etc.
    val avoid = avoidEngine.extractFrom(doc)
    println(s"${avoid.length} AVOID:")
    avoid.foreach(DisplayUtils.displayMention)
    val stateFromAvoid = State(avoid)
    // extract the base entities
    // NOTE: we have an action that prevents matching entities that overlap with an Avoid mention
    val baseEntities = entityEngine.extractFrom(doc, stateFromAvoid)
    // make sure that all are valid (i.e., contain a noun or would have contained a noun except for trigger avoidance)
    val validBaseEntities = baseEntities.filter(isValidBaseEntity)
    val splitEntities = validBaseEntities.flatMap(EntityHelper.splitCoordinatedEntities)
    // expand entities
    val expanded = PortugueseEntityFinder.expansionHandler.expand(splitEntities, maxHops = PortugueseExpansionHandler.MAX_HOPS, stateFromAvoid)
    // merge overlapping entities
    val mergedEntities = mergeOverlapping(expanded)
    //val mergedEntities = expanded
    // remove entity duplicates introduced by splitting expanded
    val distinctEntities: Seq[Mention] = mergedEntities.groupBy(m => (m.sentenceObj, m.tokenInterval, m.label)).values.toSeq.map(_.head)
    // filter entities (ex. check if case of coref)
    val filteredEntities = filterEntities(distinctEntities, state = stateFromAvoid)
    // trim unwanted POS from entity edges
    val trimmedEntities = filteredEntities.map(EntityHelper.trimEntityEdges(_, PortugueseEntityFinder.INVALID_EDGE_TAGS))
    trimmedEntities ++ avoid // display Avoid mentions in results
  }

  /** Set of filters that valid entities must satisfy */
  def filterEntities(mentions: Seq[Mention], state: State): Seq[Mention] = {
    mentions.filter(! possibleCoref(_, state))
  }

  // While the entity won't expand into a coref Avoid mention, the presence of such a mention may mean the entity captured
  // is only a fragment, which is in fact a coref mention.
  // This filter applies some dumb heuristics to drop entities that are suspected of being unresolved cases of coreference
  def possibleCoref(mention: Mention, state: State): Boolean = {
    val unresolved = state.mentionsFor(mention.sentence).filter(_.matches(PortugueseEntityFinder.AVOID_COREF_LABEL))
    if (unresolved.isEmpty) {
      false
    } else {
      mention.sentenceObj.dependencies match {
        // is there a outgoing path from the entity to at least one unresolved coref mention?
        case Some(deps) =>
          ! mention.tokenInterval.exists{ start =>
            val endIndices: Set[Int] = unresolved.flatMap{_.tokenInterval}.toSet
            endIndices.exists{ end => deps.shortestPath(start = start, end = end, ignoreDirection = false).nonEmpty }
          }
        // No dependencies?  Check for any occurrence of a coref mention
        case None      =>
          unresolved.nonEmpty
      }
    }
  }

  /** Filtering procedure designed to merge overlapping entities */
  def mergeOverlapping(mentions: Seq[Mention]): Seq[Mention] = {
    // separate out the entities
    val (entities, other) = mentions.partition(_.matches("Entity"))
    // merge overlapping entities
    val mergedEntities: Seq[Mention] = {
      // 1. determine which entities are in each sentence
      entities.groupBy(_.sentence).flatMap { case (sentenceIdx: Int, ents: Seq[Mention]) =>
        // 2. for each set of entities within a sentence...
        ents.map { entity: Mention =>
          // 2a. find overlapping entities
          val overlapping = ents.filter {
            _.tokenInterval.overlaps(entity.tokenInterval)
          }
          // 2b. merge overlapping entities
          val mergedEntity: Mention = {
            val newStart = overlapping.map(_.start).min
            val newEnd = overlapping.map(_.end).max
            // do we need to merge (i.e., do we have more than one entity in our overlapping set?
            if (entity.start == newStart && entity.end == newEnd) {
              entity
            } else {
              val ruleNames   = overlapping.sortBy(_.start).map(_.foundBy).mkString("+")
              val newFoundBy  = s"$ruleNames-mergeOverlapping"
              val newInterval = Interval(newStart, newEnd)
              val mergedAttachments = overlapping.flatMap(_.attachments).toSet
              new TextBoundMention(
                // FIXME: this assumes the labels match...if not, how should this be labeled?
                labels = entity.labels,
                tokenInterval = newInterval,
                sentence = sentenceIdx,
                document = entity.document,
                keep = entity.keep,
                foundBy = newFoundBy,
                attachments = mergedAttachments
              )
            }
          }
          mergedEntity
        }
      }
    }.toSeq
    mergedEntities ++ other
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
        (tags(entity.end).startsWith("N") || tags(entity.end).startsWith("PROPN"))
    }

    def containsValidNounVerb(entity: Mention): Boolean = {
      //val lemmas = entity.lemmas.get
      val tags = entity.tags.get
      val entities = entity.entities.get

      // Make sure there is a noun that isn't a named entity.  We can also check for stop words with some re-architecting...
      tags.indices.exists { i =>
        (tags(i).startsWith("N") || tags(i).startsWith("PROPN") || tags(i).startsWith("V")) &&
          !StopwordManager.STOP_NER.contains(entities(i))
      }
    }
    // If there's a non-named entity noun in the entity, it's valid
    containsValidNounVerb(entity) ||
      // Otherwise, if the entity ends with an adjective and the next word is a noun (which was excluded because
      // it's needed as a trigger downstream), it's valid (ex: 'economic declines')
      (entity.tags.get.last.startsWith("JJ") || entity.tags.get.last.startsWith("ADJ")) && nextTagNN(entity) ||
      // Otherwise, is it a determiner that may need to be resolved downstream?
      EidosActions.startsWithCorefDeterminer(entity)
    // Otherwise, it's not valid
  }

}

object PortugueseEntityFinder extends LazyLogging {

  val AVOID_COREF_LABEL       = "AvoidCoref"
  val DEFAULT_MAX_LENGTH: Int = RuleBasedEntityFinder.DEFAULT_MAX_LENGTH // maximum length (in tokens) for an entity

  val expansionHandler        = new PortugueseExpansionHandler

    // Set of tags that we don't want to begin or end an entity
  // FIXME: adapt to include UD tags
  val INVALID_EDGE_TAGS: Set[scala.util.matching.Regex] = Set(
    "^PRP".r,
    "^IN".r,
    "^TO".r,
    "^DT".r,
    ",".r,
    // PORTUGUESE
    "PRON".r,
    //"ADP".r,
    "DET".r
    //"[!\"#$%&'*+,-\\./:;<=>?@\\^_`{|}~]".r
  )

  def apply(entityRulesPath: String, avoidRulesPath: String, maxHops: Int, maxLength: Int = DEFAULT_MAX_LENGTH): PortugueseEntityFinder = {
    val entityRules   = FileUtils.getTextFromResource(entityRulesPath)
    val entityActions = new PortugueseActions
    val entityEngine  = ExtractorEngine(entityRules, actions = entityActions)

    val avoidRules    = FileUtils.getTextFromResource(avoidRulesPath)
    val avoidEngine   = ExtractorEngine(avoidRules)

    new PortugueseEntityFinder(entityEngine = entityEngine, avoidEngine = avoidEngine, maxHops = maxHops)
  }
}
