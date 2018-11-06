package org.clulab.wm.eidos.context

import org.clulab.wm.eidos.utils.Sourcer
import org.deeplearning4j.nn.graph.ComputationGraph
import org.deeplearning4j.nn.modelimport.keras.KerasModelImport
import org.nd4j.linalg.factory.Nd4j

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object GeoDisambiguateParser {
  val INT2LABEL = Map(0 -> "I-LOC", 1 -> "B-LOC", 2 -> "O")
  val UNKNOWN_TOKEN = "UNKNOWN_TOKEN"
  val TIME_DISTRIBUTED_1 = "time_distributed_1"
}

class GeoDisambiguateParser(modelPath: String, word2IdxPath: String, loc2geonameIDPath: String) {

  protected val network: ComputationGraph = KerasModelImport.importKerasModelAndWeights(modelPath, false)

  lazy protected val word2int: mutable.Map[String, Int] = readDict(word2IdxPath)
  lazy protected val loc2geonameID: mutable.Map[String, Int] = readDict(loc2geonameIDPath) // provide path of geoname dict file having geonameID with max population

  protected def readDict(dictPath: String): mutable.Map[String, Int] = {
    val source = Sourcer.sourceFromResource(dictPath)
    val dict = mutable.Map.empty[String, Int]

    source.getLines.foreach { line =>
      val words = line.split(' ')

      dict += (words(0).toString -> words(1).toInt)
    }
    source.close()
    dict
  }

  def makeFeatures(words: Array[String]): Array[Float] = {
    val unknown = word2int(GeoDisambiguateParser.UNKNOWN_TOKEN)
    val features = words.map(word2int.getOrElse(_, unknown).toFloat)

    features
  }

  def makeLabels(features: Array[Float]): Array[String] = {

    def argmax(values: Array[Float]): Int = {
      // This goes through the values twice, but at least it doesn't create extra objects.
      val max = values.max

      values.indexWhere(_ == max)
    }

    val input = Nd4j.create(features)
    val results = this.synchronized {
      network.setInput(0, input)
      network.feedForward()
    }
    val output = results.get(GeoDisambiguateParser.TIME_DISTRIBUTED_1)
    val predictions: Array[Array[Float]] =
        if (output.shape()(0) == 1) Array(output.toFloatVector)
        else output.toFloatMatrix

    predictions.map(prediction => GeoDisambiguateParser.INT2LABEL(argmax(prediction)))
  }

  def makeGeoLocations(labels: Array[String], words: Array[String],
      startOffsets: Array[Int], endOffsets: Array[Int]): List[GeoPhraseID] = {
    var locations = new ListBuffer[GeoPhraseID]
    var locationPhrase = ""
    var startIndex = 0

    def addLocation(index: Int) = {
      val prettyLocationPhrase = locationPhrase.replace('_', ' ')
      val geoNameId = loc2geonameID.get(locationPhrase.toLowerCase)
      // The word at index has ended previously, so use index - 1.
      val endIndex = index - 1

      locations += GeoPhraseID(prettyLocationPhrase, geoNameId, startOffsets(startIndex), endOffsets(endIndex))
    }

    for ((label, index) <- labels.zipWithIndex) {
      if (label == "B-LOC") {
        if (locationPhrase.nonEmpty)
          addLocation(index)
        locationPhrase = words(index) // initializing the location phrase with current word
        startIndex = index
      }
      else if (label == "I-LOC") {
        if (locationPhrase.nonEmpty)
          locationPhrase += ("_" + words(index))
        else {
          locationPhrase = words(index)
          startIndex = index // This case means that we are getting I-LOC but there was no B-LOC before this step.
        }
      }
      else if (label == "O") {
        if (locationPhrase.nonEmpty)
          addLocation(index)
        locationPhrase = ""
      }
    }
    if (locationPhrase.nonEmpty)
      addLocation(labels.size)
    locations.toList
  }
}
