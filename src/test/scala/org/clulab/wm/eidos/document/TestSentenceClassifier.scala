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
//  val config = EidosSystem.defaultConfig
//  val sentenceExtractor  = EidosProcessor("english", cutoff = 150)
//  val tagSet = sentenceExtractor.getTagSet
//  val stopwordManager = StopwordManager.fromConfig(config, tagSet)
//  val ontologyHandler = OntologyHandler.load(config[Config]("ontologies"), sentenceExtractor, stopwordManager, tagSet)
  val config = EidosSystem.defaultConfig
  val eidosSystem = new EidosSystem(config)  // TODO: maybe should add config.

  // TODO: Ask Keith, do we really need this class?
  // TODO: what does this "hasBeenCalled" do?
//  class TestableSentenceClassifier extends SentenceClassifier(config, ontologyHandler) {
//    var hasBeenCalled = false
//
//    override def classify(sentence: Sentence): Float = {
//      val result = super.classify(sentence)
//
//      hasBeenCalled = true
//      result
//    }
//  }

  def getEvaluationStatistics(preds:Seq[Int], labels:Seq[Int]):(Float, Float) = {
    var truePositive = 0
    var falsePositive = 0
    var trueNegative = 0
    var falseNegative = 0

    for (idx <- preds.indices){
      if (preds(idx)==1 && labels(idx)==1){
        truePositive+=1
      }
      else if (preds(idx)==1 && labels(idx)==0){
        falsePositive+=1
      }
      else if (preds(idx)==0 && labels(idx)==0){
        trueNegative +=1
      }
      else if (preds(idx)==0 && labels(idx)==1){
        falseNegative+=1
      }
    }
    val accuracy = (truePositive+trueNegative).toFloat/preds.length
    val precision = truePositive.toFloat/(truePositive+falsePositive)
    val recall = truePositive.toFloat/(truePositive+falseNegative)
    val f1 = 2*precision*recall/(precision+recall)

    (accuracy, f1)
  }

  def readEvaluationData():Seq[(String, Int)] = {
    val sentenceClassifierEvaluationData = ArrayBuffer[(String, Int)]()

    val spreadsheetPath = config.getString("sentenceClassifier.evaluationFilePath")

    //val bufferedSource = sourceFromResource(spreadsheetPath)
    val bufferedSource = SentenceClassifier.sourceFromResource(spreadsheetPath)
    for (line <- bufferedSource.getLines) {

      val cols = line.split("\t").map(_.trim)
      // do whatever you want with the columns here
      val sentence = cols(0).toLowerCase()
      val label = cols(1).toInt

      sentenceClassifierEvaluationData.append((sentence, label))
    }

    sentenceClassifierEvaluationData.toSeq
  }

  //TODO: Ask Keith, this seems not to be useful right now.
//  def newSentence(words: Array[String]): Sentence = {
//    val startOffsets = words.scanLeft(0) { case (start, word) =>  start + word.length + 1 }.dropRight(1)
//    val endOffsets = words.scanLeft(-1) { case (end, word) => end + 1 + word.length }.drop(1)
//
//    // This may need to be a more complicated sentence with tags, lemmas, etc.
//    new Sentence(words, startOffsets, endOffsets, words)
//  }

  behavior of "SentenceClassifier"

  // the
  it should "have an accuracy above 0.69 and an f1 above 0.77" in {
    val sentenceClassifierEvaluationData = readEvaluationData()
    val preds = new ArrayBuffer[Int]()
    val labels = new ArrayBuffer[Int]()
    //val sentenceClassifier = new TestableSentenceClassifier()

    // First, load the csv spread sheet
    for (i <- sentenceClassifierEvaluationData.indices) {
      val sentence = sentenceClassifierEvaluationData(i)._1
      val sentenceObj = eidosSystem.components.proc.annotate(sentence).sentences.head
      val label = sentenceClassifierEvaluationData(i)._2
      labels.append(label)


      val classifierPred = eidosSystem.components.eidosSentenceClassifier.classify(sentenceObj).get
      if (classifierPred> eidosSystem.components.eidosSentenceClassifier.classificationThreshold){
        preds.append(1)
      }
      else {
        preds.append(0)
      }
    }
    val (acc, f1) = getEvaluationStatistics(preds, labels)

    //TODO: delete this before actual merging.
    println("acc f1 are", acc, f1)
    //sentenceClassifier.hasBeenCalled should be (true)
    acc>0.69 should be (true)
    f1>0.77 should be (true)
  }


  // TODO: ask keith, are these tests useful?
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
