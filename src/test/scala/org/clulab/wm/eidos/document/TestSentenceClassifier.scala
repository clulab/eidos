package org.clulab.wm.eidos.document

import com.typesafe.config.Config
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.test.TestUtils.Test
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.Sourcer

import scala.collection.mutable.ArrayBuffer

// This isn't inheriting from EnglishTest because grounding is usually not enabled for tests.
class TestSentenceClassifier extends Test {
  // Load eidos system
  val config: Config = EidosSystem.defaultConfig
  val eidosSystem = new EidosSystem(config)

  //Get accuracy and f1 score of the predictions.
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

  // Read evaluation data from the resource folder
  def readEvaluationData():Seq[(String, Int)] = {
    val sentenceClassifierEvaluationData = ArrayBuffer[(String, Int)]()

    val spreadsheetPath = config.getString("sentenceClassifier.evaluationFilePath")

    Sourcer.sourceFromResource(spreadsheetPath).autoClose { bufferedSource =>
      for (line <- bufferedSource.getLines) {

        val cols = line.split('\t').map(_.trim)
        // do whatever you want with the columns here
        val sentence = cols(0).toLowerCase()
        val label = cols(1).toInt

        sentenceClassifierEvaluationData.append((sentence, label))
      }
    }

    sentenceClassifierEvaluationData
  }

  behavior of "SentenceClassifier"

  it should "have an accuracy above 0.69 and an f1 above 0.77" in {
    // Classification threshold can be set in the eidos.conf file.
    val classificationThreshold = eidosSystem.components.eidosSentenceClassifier.classificationThreshold
    val sentenceClassifierEvaluationData = readEvaluationData()
    val preds = new ArrayBuffer[Int]()
    val labels = new ArrayBuffer[Int]()

    for (i <- sentenceClassifierEvaluationData.indices) {
      val sentence = sentenceClassifierEvaluationData(i)._1
      val sentenceObj = eidosSystem.components.proc.annotate(sentence).sentences.head
      val label = sentenceClassifierEvaluationData(i)._2
      labels.append(label)

      val classifierPred = eidosSystem.components.eidosSentenceClassifier.classify(sentenceObj).get

      preds.append(if (classifierPred > classificationThreshold) 1 else 0)
    }
    val (acc, f1) = getEvaluationStatistics(preds, labels)

    acc>0.69 should be (true)
    f1>0.77 should be (true)
  }

}
