package org.clulab.wm.eidos.extraction

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin.{ExtractorEngine, Mention, State}
import org.clulab.processors.Document
import org.clulab.wm.eidos.actions.CorefHandler
import org.clulab.wm.eidos.expansion.Expander
import org.clulab.wm.eidos.utils.{FileUtils, StopwordManager}


class RuleBasedEntityFinder(val expander: Option[Expander], val entityEngine: ExtractorEngine, val avoidEngine: ExtractorEngine) extends Finder {
  /**
    * Performs rule-based entity extraction with selective expansion along syntactic dependencies.
    * For filtering, see filterEntities.
    * @param doc an org.clulab.processors.Document
    */
  def find(doc: Document, initialState: State = new State()): Seq[Mention] = {
    // avoid refs, etc.
    val avoid = avoidEngine.extractFrom(doc)
    val stateFromAvoid = initialState.updated(avoid)
    val baseEntities = entityEngine.extractFrom(doc, stateFromAvoid).filter{ entity => ! stateFromAvoid.contains(entity) }
    // make sure that all are valid (i.e., contain a noun or would have contained a noun except for trigger avoidance)
    val validBaseEntities = baseEntities.filter(isValidBaseEntity)
    // Optionally expand
    val expandedEntities = expander.map(_.expand(validBaseEntities, stateFromAvoid)).getOrElse(validBaseEntities)
    // split entities on likely coordinations
    val splitEntities = (validBaseEntities ++ expandedEntities).flatMap(EntityHelper.splitCoordinatedEntities)
    // remove entity duplicates introduced by splitting expanded
    val distinctEntities = splitEntities.distinct
    // trim unwanted POS from entity edges
    val trimmedEntities = distinctEntities.map(EntityHelper.trimEntityEdges)
    // if there are no avoid mentions, no need to filter
    val res = if (avoid.isEmpty) {
      trimmedEntities
    } else {
      // check that our expanded entities haven't swallowed any avoid mentions
      val avoidLabel = avoid.head.labels.last
      trimmedEntities.filter{ m => stateFromAvoid.mentionsFor(m.sentence, m.tokenInterval, avoidLabel).isEmpty }
    }
    res
  }



  def extractAndFilter(doc: Document): Seq[Mention] = {
    val entities = find(doc)
    filterEntities(entities)
  }

  /** Extracts entities without expanding or applying validation filters **/
  def extractBaseEntities(doc: Document): Seq[Mention] = {
    // avoid refs, etc.
    val avoid = avoidEngine.extractFrom(doc)
    val entities = entityEngine.extractFrom(doc, State(avoid))
    entities.filter(entity => ! avoid.contains(entity))
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
      // Otherwise, if the entity ends with an adjective and the next word is a noun (which was excluded because ]
      // it's needed as a trigger downstream), it's valid (ex: 'economic declines')
      (entity.tags.get.last.startsWith("JJ") || entity.tags.get.last.startsWith("ADJ")) && nextTagNN(entity) ||
      // Otherwise, is it a determiner that may need to be resolved downstream?
      CorefHandler.startsWithCorefDeterminer(entity) // fixme
    // Otherwise, it's not valid
  }

  /**
    * Selects longest mentions among groups of overlapping entities
    * before applying a series of filtering constraints
    * Filter criteria: PoS tag validation of final token, bracket matching, and max length
    * @param entities entities to filter
    */
  private def filterEntities(entities: Seq[Mention]): Seq[Mention] = {
    // ignore citations and remove any entity that is too long given our criteria
    val filteredEntities = entities.filter(m => EntityConstraints.withinMaxLength(m, RuleBasedEntityFinder.DEFAULT_MAX_LENGTH)) // FIXME
    val longest = keepLongest(filteredEntities, new State())
    for {
      m <- longest
      if EntityConstraints.validFinalTag(m)
      if EntityConstraints.matchingBrackets(m)
    } yield m
  }


  /** Keeps the longest mention for each group of overlapping mentions **/
  def keepLongest(mentions: Seq[Mention], state: State = new State()): Seq[Mention] = {
    val mns: Iterable[Mention] = for {
      // find mentions of the same label and sentence overlap
      (k, v) <- mentions.groupBy(m => (m.sentence, m.label))
      m <- v
      // for overlapping mentions starting at the same token, keep only the longest
      longest = v.filter(_.tokenInterval.overlaps(m.tokenInterval)).maxBy(m => m.end - m.start)
    } yield longest
    mns.toVector.distinct
  }
}

object RuleBasedEntityFinder {
  val DEFAULT_MAX_LENGTH = 50 // maximum length (in tokens) for an entity // FIXME read in?

  def fromConfig(config: Config): RuleBasedEntityFinder = {
    val entityRulesPath = config[String]("ruleBasedEntityFinder.entityRulesPath")
    val entityRules = FileUtils.getTextFromResource(entityRulesPath)
    val entityEngine = ExtractorEngine(entityRules)

    val avoidRulesPath = config[String]("ruleBasedEntityFinder.avoidRulesPath")
    val avoidRules = FileUtils.getTextFromResource(avoidRulesPath)
    val avoidEngine = ExtractorEngine(avoidRules)

    val expanderConfig = config.get[Config]("ruleBasedEntityFinder.expander")
    val expander: Option[Expander] = expanderConfig.map(Expander.fromConfig)

    new RuleBasedEntityFinder(expander, entityEngine, avoidEngine)
  }
}