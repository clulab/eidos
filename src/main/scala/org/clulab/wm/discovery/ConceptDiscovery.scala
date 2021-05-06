package org.clulab.wm.discovery

import org.clulab.dynet.Utils
import org.clulab.wm.eidos.extraction.RuleBasedEntityFinder
import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.processors.clucore.CluCoreProcessor
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.extraction.EntityHelper
import org.clulab.wm.eidoscommon.EnglishTagSet
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidos.utils.StopwordManager

import java.nio.file.Path
import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import org.clulab.embeddings.{ExplicitWordEmbeddingMap, WordEmbeddingMapPool}
import org.jgrapht.graph._
import org.jgrapht.alg.scoring.PageRank

case class Concept(phrase: String, documentLocations: Set[String]) {
  def frequency: Int = documentLocations.size
}

case class RankedConcept(concept: Concept, saliency: Double)

class ConceptDiscovery {

  def discoverConcepts(documents: Seq[Path]): Set[Concept] = {
    Utils.initializeDyNet()
    val tagSet = new EnglishTagSet()
    val Config = EidosSystem.defaultConfig
    val stopwordManager = StopwordManager.fromConfig(Config, tagSet)
    val config = ConfigFactory.load("reference")
    val entityFinder = RuleBasedEntityFinder.fromConfig(config, tagSet, stopwordManager)
    val processor = new CluCoreProcessor()
    val conceptLocations = mutable.Map.empty[String, Set[String]].withDefaultValue(Set.empty)
    for (path <- documents) {
      val text = FileUtils.getTextFromFile(path.toString)
      val document = processor.annotate(text)
      val mentions = entityFinder.find(document)
      val trimmed_mentions = mentions.map(EntityHelper.trimEntityEdges(_, tagSet))
      val annotatedDocument = AnnotatedDocument(document, trimmed_mentions)
      for (mention <- annotatedDocument.odinMentions) {
        conceptLocations(mention.text) += s"${path}:${mention.sentence}"
      }
    }
    Set.empty ++ conceptLocations.map {
      case (phrase, documentLocations) => Concept(phrase, documentLocations)
    }
  }

  def build_graph(concepts: HashMap[String, Double], threshold: Double, embedding:ExplicitWordEmbeddingMap): SimpleWeightedGraph[String, DefaultEdge] = {
    val g = new SimpleWeightedGraph[String, DefaultEdge](classOf[DefaultEdge])
    val pairs = concepts.keySet.toList.combinations(2).toList
    for (x <- pairs) {
      val x1 = x(0)
      val x2 = x(1)
      if (!g.containsVertex(x1)) {
        g.addVertex(x1)
      }
      if (!g.containsVertex(x2)) {
        g.addVertex(x2)
      }
      val weight = embedding.avgSimilarity(x1.split(' '), x2.split(' '))
      if (weight > threshold) {
        if (!g.containsEdge(x1, x2)) {
          val e = g.addEdge(x1, x2)
          g.setEdgeWeight(e, weight)
        }
      }
    }
    g
  }

  def rankConcepts(concepts: Set[Concept], threshold_frequency: Double, threshold_similarity: Double, top_pick: Int): Seq[RankedConcept] = {
    val temp = new HashMap[String, Double]()
    val embed_file = getClass.getResource("vectors.txt").getPath
    val wordEmbeddings = WordEmbeddingMapPool.getOrElseCreate(embed_file, compact = false).asInstanceOf[ExplicitWordEmbeddingMap]
    for (concept <- concepts){
      val phrase = concept.phrase
      temp(phrase) = concept.frequency
    }
    val url_starts = List("www", "http")
    val url_ends = List(".com", ".org", ".edu")
    val stop_words = List("a", "an", "and", "are", "as", "at", "be", "but", "by", "for", "if", "in", "into", "is", "it", "no", "not", "of", "on", "or", "such", "that", "the", "their", "then", "there", "these", "they", "this", "to", "was", "will", "with")
    val filtered_concepts = temp.filter(x => !url_starts.exists(y => x._1.startsWith(y)) && !url_ends.exists(y => x._1.endsWith(y)) && x._2 > threshold_frequency && !stop_words.contains(x._1))

    val topn = filtered_concepts.toSeq.sortWith(_._2 > _._2).toList.take(top_pick).map(i => i._1 -> i._2)
    val concept_topn = new HashMap[String, Double]
    topn.foreach(p => concept_topn.put(p._1, p._2))
    val g = build_graph(concept_topn, threshold_similarity, wordEmbeddings)
    val pr = new PageRank(g)
    val score_map = pr.getScores
    val ranked_concepts = ListBuffer[RankedConcept]()
    for (concept<-concepts){
      val phrase = concept.phrase
      if (score_map.containsKey(phrase))
        ranked_concepts += RankedConcept(concept, score_map.get(phrase))
    }
    ranked_concepts
  }
}

