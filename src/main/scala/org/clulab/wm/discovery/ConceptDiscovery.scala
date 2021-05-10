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
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import org.clulab.embeddings.{CompactWordEmbeddingMap, WordEmbeddingMapPool}
import org.jgrapht.graph._
import org.jgrapht.alg.scoring.PageRank
import ai.lum.common.ConfigUtils._
import org.clulab.utils.StringUtils

case class CdrDocument(docid: String, sentences: Seq[ScoredSentence])
case class ScoredSentence(text: String, start: Int, end: Int, score: Double)


case class Concept(phrase: String, documentLocations: Set[String]) {
  def frequency: Int = documentLocations.size
}

case class RankedConcept(concept: Concept, saliency: Double)

class ConceptDiscovery {

  def discoverConcepts(cdrs: Seq[CdrDocument], sentenceThreshold: Option[Double] = None): Set[Concept] = {
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

  def rankConcepts(concepts: Set[Concept], threshold_frequency: Double, threshold_similarity: Double, top_pick: Int): Seq[RankedConcept] = {
    val config = ConfigFactory.load("glove")
    val embed_file_path:String = config[String]("glove.matrixResourceName")
    val wordEmbeddings = WordEmbeddingMapPool.getOrElseCreate(embed_file_path, compact = true).asInstanceOf[CompactWordEmbeddingMap]
    val stop_words = Set("a", "an", "and", "are", "as", "at", "be", "but", "by", "for", "if", "in", "into", "is", "it", "no", "not", "of", "on", "or", "such", "that", "the", "their", "then", "there", "these", "they", "this", "to", "was", "will", "with")

    // select top k concepts
    val selectedConcepts = concepts.filter(c => c.frequency > threshold_frequency && !stop_words.contains(c.phrase)).toSeq.sortBy(-_.frequency).take(top_pick)

    // construct graph from concepts
    val g = new SimpleWeightedGraph[String, DefaultEdge](classOf[DefaultEdge])
    for (concept <- selectedConcepts) {
      g.addVertex(concept.phrase)
    }
    for (List(c1, c2) <- selectedConcepts.combinations(2)) {
      val weight = wordEmbeddings.avgSimilarity(c1.phrase.split(' '), c2.phrase.split(' '))
      if (weight > threshold_similarity && !g.containsEdge(c1.phrase, c2.phrase)) {
        val e = g.addEdge(c1.phrase, c2.phrase)
        g.setEdgeWeight(e, weight)
      }
    }

    // add PageRank scores to each concept
    val pr = new PageRank(g)
    selectedConcepts.map(c => RankedConcept(c, pr.getVertexScore(c.phrase)))
  }
}

