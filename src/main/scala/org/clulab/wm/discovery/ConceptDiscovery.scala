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
      val sentencesOverThresholdOption = sentenceThreshold.map(t => cdr.sentences.filter(_.score >= t))
      val sentences = sentencesOverThresholdOption.getOrElse(cdr.sentences)
      val document = processor.annotateFromSentences(sentences.map(_.text))
      // find and collect concept mentions
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

  def rankConcepts(concepts: Set[Concept]): Seq[RankedConcept] = ???

}

