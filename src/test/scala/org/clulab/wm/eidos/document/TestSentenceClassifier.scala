package org.clulab.wm.eidos.document

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.jsonld.JLDCorpus
import org.clulab.wm.eidos.serialization.jsonld.JLDDeserializer
import org.clulab.wm.eidos.test.TestUtils.Test

import scala.collection.mutable.ArrayBuffer

class TestSentenceClassifier extends Test {

  //Load eidos system
  val config = EidosSystem.defaultConfig
  val eidosSystem = new EidosSystem(config)
  // Classification threshold can be set in the eidos.conf file.
  val classificationThreshold = eidosSystem.components.eidosSentenceClassifier.classificationThreshold

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

    val bufferedSource = SentenceClassifier.sourceFromResource(spreadsheetPath)
    for (line <- bufferedSource.getLines) {

      val cols = line.split("\t").map(_.trim)
      // do whatever you want with the columns here
      val sentence = cols(0).toLowerCase()
      val label = cols(1).toInt

      sentenceClassifierEvaluationData.append((sentence, label))
    }

    sentenceClassifierEvaluationData
  }

  behavior of "SentenceClassifier"

  it should "have an accuracy above 0.69 and an f1 above 0.77" in {
    val sentenceClassifierEvaluationData = readEvaluationData()
    val preds = new ArrayBuffer[Int]()
    val labels = new ArrayBuffer[Int]()

    for (i <- sentenceClassifierEvaluationData.indices) {
      val sentence = sentenceClassifierEvaluationData(i)._1
      val sentenceObj = eidosSystem.components.proc.annotate(sentence).sentences.head
      val label = sentenceClassifierEvaluationData(i)._2
      labels.append(label)

      val classifierPred = eidosSystem.components.eidosSentenceClassifier.classify(sentenceObj).get

      // Classification threshold can be set in the eidos.conf file.
      if (classifierPred> classificationThreshold){
        preds.append(1)
      }
      else {
        preds.append(0)
      }
    }
    val (acc, f1) = getEvaluationStatistics(preds, labels)

    acc>0.69 should be (true)
    f1>0.77 should be (true)
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
