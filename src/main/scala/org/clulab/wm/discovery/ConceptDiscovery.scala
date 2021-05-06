package org.clulab.wm.discovery

import org.clulab.dynet.Utils
import org.clulab.wm.eidos.extraction.RuleBasedEntityFinder
import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.processors.clucore.CluCoreProcessor
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.extraction.EntityHelper
import org.clulab.wm.eidoscommon.EnglishTagSet
import org.clulab.wm.eidos.utils.StopwordManager
import org.clulab.processors.{Document, Processor}

import scala.collection.mutable.ArrayBuffer

case class CdrDocument(text: String, docid: String, sentenceRankings: Seq[SentenceScore])
case class SentenceScore(start: Int, end: Int, score: Double)

case class Concept(phrase: String, frequency: Int, documentLocations: Set[String])

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
    val textListAll = new ArrayBuffer[String]()
    val sentenceListAll = new ArrayBuffer[String]()
    for ((cdr, idx) <- cdrs.zipWithIndex) {
      // make a Processors Document, pruning sentences with a threshold if applicable
      val document = mkDocument(processor, cdr.text, sentenceThreshold, cdr.sentenceRankings)
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

  /** Currently pruning sentences from the text whose Qntfy sentence score are below a threshold.
    * TODO: should this be a top k instead? */
  def mkDocument(processor: Processor, text: String, sentenceThreshold: Option[Double], rankedSentences: Seq[SentenceScore]): Document = {
    if (sentenceThreshold.isEmpty) {
      processor.annotate(text)
    }
    else {
      val document = processor.mkDocument(text)
      val lastValid = rankedSentences.indexWhere(_.score < sentenceThreshold)
      // only accept sentences with a saliency above some threshold
      val validSentences = rankedSentences.slice(0, lastValid)
      val sentencesByOffsets = document.sentences.map(s => ((s.startOffsets.head, s.endOffsets.last), s)).toMap
      val keptSentences = validSentences
        // keep ones that are in the validSentences
        .map(s => sentencesByOffsets((s.start, s.end)))
        // put them back in order
        .sortBy(_.startOffsets.head)
        // get their text
        .map(_.getSentenceText)
        .toArray
      // annotate and return as a Document
      processor.annotateFromSentences(keptSentences)
    }
  }

  def rankConcepts(concepts: Set[Concept]): Seq[RankedConcept] = ???

}

