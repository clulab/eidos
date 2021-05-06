package org.clulab.wm.discovery

import org.clulab.dynet.Utils
import org.clulab.wm.eidos.extraction.RuleBasedEntityFinder
import com.typesafe.config.ConfigFactory
import org.clulab.processors.clucore.CluCoreProcessor
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.extraction.EntityHelper
import org.clulab.wm.eidoscommon.EnglishTagSet
import org.clulab.wm.eidos.utils.StopwordManager
import org.clulab.processors.{Document, Processor}
import scala.collection.mutable

case class CdrDocument(docid: String, sentences: Seq[ScoredSentence])
case class ScoredSentence(text: String, start: Int, end: Int, score: Double)


case class Concept(phrase: String, documentLocations: Set[String]) {
  def frequency: Int = documentLocations.size
}

case class RankedConcept(concept: Concept, saliency: Double)

class ConceptDiscovery {

  def discoverConcepts(cdrs: Seq[CdrDocument], sentenceThreshold: Option[Double]): Set[Concept] = {
    Utils.initializeDyNet()
    val tagSet = new EnglishTagSet()
    val Config = EidosSystem.defaultConfig
    val stopwordManager = StopwordManager.fromConfig(Config, tagSet)
    val config = ConfigFactory.load("reference")
    val entityFinder = RuleBasedEntityFinder.fromConfig(config, tagSet, stopwordManager)
    val processor = new CluCoreProcessor()
    val conceptLocations = mutable.Map.empty[String, Set[String]].withDefaultValue(Set.empty)
    for (cdr <- cdrs) {
      // make a Processors Document, pruning sentences with a threshold if applicable
      val document = mkDocument(processor, cdr.sentences, sentenceThreshold)
      val mentions = entityFinder.find(document)
      val trimmed_mentions = mentions.map(EntityHelper.trimEntityEdges(_, tagSet))
      val annotatedDocument = AnnotatedDocument(document, trimmed_mentions)
      for (mention <- annotatedDocument.odinMentions) {
        conceptLocations(mention.text) += s"${cdr.docid}:${mention.sentence}"
      }
    }

    conceptLocations.map{
      case (phrase, locations) => Concept(phrase, locations)
    }.toSet
  }

  /** Currently pruning sentences from the text whose Qntfy sentence score are below a threshold.
    * TODO: should this be a top k instead? */
  def mkDocument(processor: Processor, sentences: Seq[ScoredSentence], sentenceThreshold: Option[Double]): Document = {
    if (sentenceThreshold.isEmpty) {
      processor.mkDocumentFromSentences(sentences.map(_.text))
    }
    else {
      // ranked in descending order of score
      val rankedSentences = sentences.sortBy(-_.score)

      // only accept sentences with a saliency above some threshold
      val lastValid = rankedSentences.indexWhere(_.score < sentenceThreshold.get)
      val validSentences = rankedSentences
        .slice(0, lastValid)
        // put them back in order
        .sortBy(_.start)

      // annotate and return as a Document
      processor.annotateFromSentences(validSentences.map(_.text))
    }
  }

  def rankConcepts(concepts: Set[Concept]): Seq[RankedConcept] = ???

}

