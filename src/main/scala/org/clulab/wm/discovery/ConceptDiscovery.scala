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

  def rankConcepts(concepts: Set[Concept]): Seq[RankedConcept] = ???

}

