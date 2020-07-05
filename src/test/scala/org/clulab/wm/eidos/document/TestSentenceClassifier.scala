package org.clulab.wm.eidos.document

import java.nio.charset.StandardCharsets

import org.clulab.processors.Sentence
import org.clulab.wm.eidos.{EidosEnglishProcessor, EidosProcessor, EidosSystem}
import org.clulab.wm.eidos.groundings.OntologyHandler
import org.clulab.wm.eidos.test.TestUtils.Test
import org.clulab.wm.eidos.utils.{Language, StopwordManager}
import com.typesafe.config.Config
import ai.lum.common.ConfigUtils._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source


class TestSentenceClassifier extends Test {
  //TODO: think of a way to use the existing eidos system when instantiating the test object.
  val config = EidosSystem.defaultConfig
  val sentenceExtractor  = EidosProcessor("english", cutoff = 150)
  val tagSet = sentenceExtractor.getTagSet
  val stopwordManager = StopwordManager.fromConfig(config, tagSet)
  val ontologyHandler = OntologyHandler.load(config[Config]("ontologies"), sentenceExtractor, stopwordManager, tagSet)

  class TestableSentenceClassifier extends SentenceClassifier(config, ontologyHandler) {
    // TODO: Put real code into SentenceClassifier, but test code here.
    var hasBeenCalled = false

    override def classify(sentence: Sentence): Float = {
      val result = super.classify(sentence)

      hasBeenCalled = true
      result
    }
  }

  def readEvaluationData():Seq[(String, Int)] = {
    val utf8: String = StandardCharsets.UTF_8.toString

    val sentenceClassifierEvaluationData = ArrayBuffer[(String, Int)]()

    //TODO: read this from resource later
    val spreadsheetPath = "/Users/zhengzhongliang/NLP_Research/2020_WorldModeler/20200703/SentenceClassifierEvaluation.tsv"

    //val bufferedSource = sourceFromResource(spreadsheetPath)
    val bufferedSource = Source.fromFile(spreadsheetPath, utf8)
    for (line <- bufferedSource.getLines) {

      val cols = line.split("\t").map(_.trim)
      // do whatever you want with the columns here
      val sentence = cols(0).toLowerCase()
      val label = cols(1).toInt

      sentenceClassifierEvaluationData.append((sentence, label))
    }

    sentenceClassifierEvaluationData.toSeq
  }

  //TODO: this seems not to be useful right now.
//  def newSentence(words: Array[String]): Sentence = {
//    val startOffsets = words.scanLeft(0) { case (start, word) =>  start + word.length + 1 }.dropRight(1)
//    val endOffsets = words.scanLeft(-1) { case (end, word) => end + 1 + word.length }.drop(1)
//
//    // This may need to be a more complicated sentence with tags, lemmas, etc.
//    new Sentence(words, startOffsets, endOffsets, words)
//  }

  behavior of "SentenceClassifier"

  // the
  it should "the accuraccy of sentence classifier should be above 0.67" in {
    val sentenceClassifierEvaluationData = readEvaluationData()
    val sentenceClassifier = new TestableSentenceClassifier()

    // First, load the csv spread sheet

    println("ontology handler loaded")

    var correctCount = 0f
    for (i <- sentenceClassifierEvaluationData.indices) {
      val sentence = sentenceClassifierEvaluationData(i)._1
      val sentenceObj = sentenceExtractor.annotate(sentence).sentences.head
      val label = sentenceClassifierEvaluationData(i)._2


      val classifierPred = sentenceClassifier.classify(sentenceObj)
      if (classifierPred>0.7 & label==1){
        correctCount+=1
      }
      else if (classifierPred<=0.7 & label==0){
        correctCount+=1
      }

    }

    sentenceClassifier.hasBeenCalled should be (true)
    correctCount/sentenceClassifierEvaluationData.length>0.67 should be (true)
  }

//  ignore should "work integrated" in {
//    val sentenceClassifier = new TestableSentenceClassifier()
//    val eidosEnglishProcessor = new EidosEnglishProcessor(Language.ENGLISH, 100)
//
//    eidosEnglishProcessor.extractDocument("This is a test.")
//    sentenceClassifier.hasBeenCalled should be (true)
//  }
//
//  ignore should "work configured" in {
//    val eidosSystem = new EidosSystem(defaultConfig)
//
//    eidosSystem.annotate("This is a test.")
//    // Just don't crash for now.
//    // Later look for side effect in document attachments.
//  }
}
