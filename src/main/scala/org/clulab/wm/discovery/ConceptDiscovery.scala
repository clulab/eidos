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

import java.io.{File, PrintWriter}
import java.nio.file.Path
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class Concept(phrase: String, frequency: Int, documentLocations: Set[String])

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
    val textListAll = new ArrayBuffer[String]()
    val sentenceListAll = new ArrayBuffer[String]()
    for ((textDir,idx) <- documents.zipWithIndex) {
      val text = FileUtils.getTextFromFile(textDir.toString)
      val document = processor.annotate(text)
      val mentions = entityFinder.find(document)
      val trimmed_mentions = mentions.map(EntityHelper.trimEntityEdges(_, tagSet))
      val annotatedDocument = AnnotatedDocument(document, trimmed_mentions)
      val textList = annotatedDocument.odinMentions.map{x => x.text}
      textListAll.appendAll(textList)
      val sentenceList = annotatedDocument.odinMentions.map{x => x.sentenceObj.getSentenceText}
      sentenceListAll.appendAll(sentenceList)
    }
    val textListCount = scala.collection.mutable.Map[String, Int]()
    for (x <- textListAll) {
      if (textListCount.contains(x)) {
        textListCount(x)+=1
      }
      else {
        textListCount(x) = 1
      }
    }
    val text2SentenceCount = scala.collection.mutable.Map[String, ArrayBuffer[String]]()
    for ((y, idx) <- textListAll.zipWithIndex) {
      if (text2SentenceCount.contains(y)) {
        text2SentenceCount(y).append(sentenceListAll(idx).replaceAll("\t"," "))
      }
      else {
        text2SentenceCount(y) = ArrayBuffer[String](sentenceListAll(idx).replaceAll("\t"," "))
      }
    }
    val conceptSet = textListCount.keys.map{x => Concept(x, textListCount(x), text2SentenceCount(x).toSet)}.toSet
    conceptSet
  }

  def rankConcepts(concepts: Set[Concept]): Seq[RankedConcept] = ???

}

