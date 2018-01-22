package org.clulab.wm.entities

import com.typesafe.scalalogging.LazyLogging
import org.clulab.openie.entities.RuleBasedEntityFinder
import org.clulab.odin.{ExtractorEngine, Mention, State, TextBoundMention}
import org.clulab.openie.ResourceUtils
import org.clulab.processors.Document
import org.clulab.struct.Interval
import org.clulab.wm.wmutils.FileUtils.readRules

import scala.annotation.tailrec

class AgroEntityFinder(
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

    // if there are no avoid mentions, no need to filter
    val res = if (avoid.isEmpty) {
      distinctEntities
    } else {
      val avoidLabel = avoid.head.labels.last
      distinctEntities.filter{ m => stateFromAvoid.mentionsFor(m.sentence, m.tokenInterval, avoidLabel).isEmpty }
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
    "^acl$".r, // replaces vmod
    // Changed from processors......
    "^nmod".r, // replaces prep_
    "case".r
  )

}

object AgroEntityFinder extends LazyLogging {

  val DEFAULT_MAX_LENGTH = 10 // maximum length (in tokens) for an entity
  def apply(maxHops: Int, maxLength: Int = DEFAULT_MAX_LENGTH): AgroEntityFinder = {
    val entityRules = ResourceUtils.readResource("org/clulab/openie/entities/grammar/entities.yml")
    val avoidRules = readRules("/org/clulab/wm/grammars/avoidLocal.yml")

    val avoidEngine = ExtractorEngine(avoidRules)
    val entityEngine = ExtractorEngine(entityRules)
    new AgroEntityFinder(avoidEngine = avoidEngine, entityEngine = entityEngine, maxHops = maxHops)
  }


}
