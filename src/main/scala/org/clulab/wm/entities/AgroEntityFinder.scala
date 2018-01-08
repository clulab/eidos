package org.clulab.wm.entities

import com.typesafe.scalalogging.LazyLogging
import org.clulab.openie.entities.RuleBasedEntityFinder
import org.clulab.odin.{ExtractorEngine, Mention, State, TextBoundMention}
import org.clulab.openie.ResourceUtils
import org.clulab.processors.Document
import org.clulab.struct.Interval
import org.clulab.wm.wmutils.FileUtils.readRules

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

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
//    println("Avoid entities:\n")
//    println(s"${avoid.sortBy(_.start).foreach(e => println(s"${e.label} (${e.foundBy}): ${e.text} (${e.start}, ${e.end})"))}")
    val stateFromAvoid = State(avoid)
    val baseEntities = entityEngine.extractFrom(doc, stateFromAvoid).filter{ entity => ! stateFromAvoid.contains(entity) }
//    println("Before expansion:\n")
//    println(s"${baseEntities.sortBy(_.start).foreach(e => println(s"${e.label} (${e.foundBy}): ${e.text} (${e.start}, ${e.end})"))}")
    val expandedEntities: Seq[Mention] = baseEntities.map(entity => expand(entity, maxHops, stateFromAvoid))
//     split entities on likely coordinations
//    println("\nAfter expansion:\n")
//    println(s"${expandedEntities.sortBy(_.start).foreach(e => println(s"${e.label} (${e.foundBy}): ${e.text} (${e.start}, ${e.end})"))}")
    val splitEntities = (baseEntities ++ expandedEntities).flatMap(splitCoordinatedEntities)
//    println("\nafter splitting:\n")
//    println(s"${splitEntities.sortBy(_.start).foreach(e => println(s"${e.label} (${e.foundBy}): ${e.text} (${e.start}, ${e.end})"))}")
    // remove entity duplicates introduced by splitting expanded
    val distinctEntities = splitEntities.distinct
//    println("\ndistinct:\n")
//    println(s"${distinctEntities.sortBy(_.start).foreach(e => println(s"${e.label} (${e.foundBy}): ${e.text} (${e.start}, ${e.end})"))}")

    // if there are no avoid mentions, no need to filter
    val res = if (avoid.isEmpty) {
//      println("No avoid mentions...")
      distinctEntities
    } else {
//      println("Avoid mentions...")
      // check that our expanded entities haven't swallowed any avoid mentions
      val avoidLabel = avoid.head.labels.last
//      println(s"avoidLabel: ${avoidLabel}")
      distinctEntities.filter{ m => stateFromAvoid.mentionsFor(m.sentence, m.tokenInterval, avoidLabel).isEmpty }
    }
//    println("\nres:\n")
//    println(s"${res.sortBy(_.start).foreach(e => println(s"${e.label} (${e.foundBy}): '${e.text} (${e.start}, ${e.end})"))}")

    res
  }

  /**
    * Expands an entity up to the specified number of hops along valid grammatical relations.
    */
  def expand(entity: Mention, maxHops: Int, stateFromAvoid: State): Mention = {
//    println(s"checking to see if I should expand '${entity.text}'... max hops=${maxHops}")
    val interval = traverseOutgoingLocal(entity, maxHops, stateFromAvoid)
//    println(s"** EXPANDED to: $interval")
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
//    println(s"    traversing recursively... tokens: ${tokens.mkString(", ")}\tnewTokens: ${newTokens.mkString(", ")}\tremaininghops=${remainingHops}")
    if (remainingHops == 0) {
      val allTokens = tokens ++ newTokens
//      println(s"      no more hops, exiting with interval: ${Interval(allTokens.min, allTokens.max + 1)}")
      Interval(allTokens.min, allTokens.max + 1)
    } else {
//      println(s"      Hops remain, checking...")
      val newNewTokens = new ArrayBuffer[Int]
      for (tok <- newTokens) {
//        println(s"        Hops remain, checking tok [${tok}]...")
        //tok <- newTokens
        if (outgoingRelations.nonEmpty && tok < outgoingRelations.length){
//          println(s"        outgoing nonempty and tok < outgoing.len...")
          for ((nextTok, dep) <- outgoingRelations(tok)) {
//            println(s"        (checking nextTok [${nextTok}] with dep: [${dep}]")
//            println("AVOIDS: " + stateFromAvoid.mentionsFor(sent, nextTok).map(_.text).mkString(", "))
            if (isValidOutgoingDependency(dep) && stateFromAvoid.mentionsFor(sent, nextTok).isEmpty) {
//              println(s"        [${dep}] is valid ... next we check incoming...")
              if (hasValidIncomingDependencies(nextTok, incomingRelations)) {
//                println(s"        nextTok [${nextTok}] has Valid Incoming... appending!")
                newNewTokens.append(nextTok)
              }
            }

          }
        }
      }
//      println(s"        -> newNewTokens: ${newNewTokens.mkString(", ")}")
      traverseOutgoingLocal(tokens ++ newTokens, newNewTokens.toSet, outgoingRelations, incomingRelations, remainingHops - 1, sent, stateFromAvoid)
    }
  }
  private def traverseOutgoingLocal(m: Mention, numHops: Int, stateFromAvoid: State): Interval = {
//    println(s"  traversing outgoing... numhops=${numHops}")
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
//    "^nmod_".r, // replaces prep_
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
