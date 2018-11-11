package org.clulab.wm.eidos.portuguese.entities

import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin.{ExtractorEngine, Mention, State}
import org.clulab.processors.Document
import org.clulab.wm.eidos.EidosActions
import org.clulab.wm.eidos.entities.{EntityHelper, RuleBasedEntityFinder}
import org.clulab.wm.eidos.portuguese.actions.PortugueseActions
import org.clulab.wm.eidos.utils.{FileUtils, StopwordManager}


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
    val stateFromAvoid = State(avoid)
    // extract the base entities
    val baseEntities = entityEngine.extractFrom(doc, stateFromAvoid).filter{ entity => ! stateFromAvoid.contains(entity) }
    // make sure that all are valid (i.e., contain a noun or would have contained a noun except for trigger avoidance)
    val validBaseEntities = baseEntities.filter(isValidBaseEntity)
    val splitEntities = validBaseEntities.flatMap(EntityHelper.splitCoordinatedEntities)
    // remove entity duplicates introduced by splitting expanded
    val distinctEntities = splitEntities.distinct
    // trim unwanted POS from entity edges
    val trimmedEntities = distinctEntities.map(EntityHelper.trimEntityEdges(_, PortugueseEntityFinder.INVALID_EDGE_TAGS))
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
  val DEFAULT_MAX_LENGTH = RuleBasedEntityFinder.DEFAULT_MAX_LENGTH // maximum length (in tokens) for an entity

  // Set of tags that we don't want to begin or end an entity
  // FIXME: adapt to include UD tags
  val INVALID_EDGE_TAGS = Set[scala.util.matching.Regex](
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

    val avoidRules = FileUtils.getTextFromResource(avoidRulesPath)
    val avoidEngine = ExtractorEngine(avoidRules)

    new PortugueseEntityFinder(entityEngine = entityEngine, avoidEngine = avoidEngine, maxHops = maxHops)
  }
}
