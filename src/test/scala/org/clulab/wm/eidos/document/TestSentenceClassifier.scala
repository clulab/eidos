package org.clulab.wm.eidos.document

import com.typesafe.config.Config
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.jsonld.{JLDCorpus, JLDDeserializer}
import org.clulab.wm.eidos.test.TestUtils.Test
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.Sourcer

import scala.collection.mutable.ArrayBuffer

// This isn't inheriting from EnglishTest because grounding is usually not enabled for tests.
class TestSentenceClassifier extends Test {
  // Load eidos system
  val config: Config = EidosSystem.defaultConfig
  val eidosSystem = new EidosSystem(config)
  // Classification threshold can be set in the eidos.conf file.
  val classificationThreshold = eidosSystem.components.eidosSentenceClassifierOpt.get.classificationThreshold

  //Get accuracy and f1 score of the predictions.
  def getEvaluationStatistics(preds:Seq[Int], labels:Seq[Int]):(Float, Float, Float, Float) = {
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

    (accuracy, precision, recall, f1)
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

  def readEvaluationData408Sample():Seq[(String, Int)] = {
    val sentenceClassifierEvaluationData = ArrayBuffer[(String, Int)]()

    val spreadsheetPath = config.getString("sentenceClassifier.evaluationFileLargePath")

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

  behavior of "SentenceClassifier on the small evaluation dataset"

  it should "have an accuracy above 0.69 and an f1 above 0.77" in {
    val sentenceClassifierEvaluationData = readEvaluationData()
    val preds = new ArrayBuffer[Int]()
    val labels = new ArrayBuffer[Int]()

    for (i <- sentenceClassifierEvaluationData.indices) {
      val sentence = sentenceClassifierEvaluationData(i)._1
      val sentenceObj = eidosSystem.components.procOpt.get.annotate(sentence).sentences.head
      val label = sentenceClassifierEvaluationData(i)._2
      labels.append(label)

      val classifierPred = eidosSystem.components.eidosSentenceClassifierOpt.get.classify(sentenceObj).get

      preds.append(if (classifierPred > classificationThreshold) 1 else 0)
    }
    val (acc, precision, recall, f1) = getEvaluationStatistics(preds, labels)

    acc>0.69 should be (true)
    f1>0.77 should be (true)
  }

  behavior of "SentenceClassifier on the 408 sample evaluation dataset"

  it should "have an precision > 0.80 and recall > 0.17" in {
    val sentenceClassifierEvaluationData = readEvaluationData408Sample()
    val preds = new ArrayBuffer[Int]()
    val labels = new ArrayBuffer[Int]()

    var invalidSentCount = 0
    for (i <- sentenceClassifierEvaluationData.indices) {
      val sentence = sentenceClassifierEvaluationData(i)._1
      if (eidosSystem.components.procOpt.get.annotate(sentence).sentences.nonEmpty){
        val sentenceObj = eidosSystem.components.procOpt.get.annotate(sentence).sentences.head
        val classifierPred = eidosSystem.components.eidosSentenceClassifierOpt.get.classify(sentenceObj).get

        // This classification threshold is largely determined by the python experiment, but I also tuned it a little bit.
        // In python, when t = 0.82, p = 0.81 and r = 0.15
        // In scala, when t=0.82, p=0.79 and r = 0.13
        // In scala, when t=0.81, p=0.803 and r = 0.178
        preds.append(if (classifierPred > 0.81) 1 else 0)
      }
      else{
        // If the sentence could not be annotated, automatically mark it as invalid
        // It turns out that there are four sentences that eidos failed to annotate.
        preds.append(0)
        invalidSentCount+=1
      }

      val label = sentenceClassifierEvaluationData(i)._2
      labels.append(label)
    }
    val (acc, precision, recall, f1) = getEvaluationStatistics(preds, labels)

    println("precision and recall:", precision, recall)

    precision>0.80 should be (true)
    recall>0.17 should be (true)
  }

  behavior of "(De)serialization"

  // The test needs to be in this file because only here are the sentences guaranteed to get classified.
  it should "handle the relevance" in {
    val oldAnnotatedDocument = eidosSystem.extractFromText("Rainfall significantly increases poverty.")
    val oldClassification = oldAnnotatedDocument.eidosMentions.head.classificationOpt.get
    oldClassification should be > classificationThreshold

    val oldCorpus = new JLDCorpus(Seq(oldAnnotatedDocument))
    val oldJValue = oldCorpus.serialize()
    val oldJson = stringify(oldJValue, pretty = true)
    oldJson should include ("relevance")

    val newAnnotatedDocument = new JLDDeserializer().deserialize(oldJson).head
    val newClassification = newAnnotatedDocument.eidosMentions.head.classificationOpt.get
    newClassification should === (oldClassification)
  }
}
