package org.clulab.wm.eidos.context

import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.Sourcer
import org.deeplearning4j.nn.graph.ComputationGraph
import org.deeplearning4j.nn.modelimport.keras.KerasModelImport
import org.nd4j.linalg.factory.Nd4j

object GeoDisambiguateParser {
  val I_LOC = 0
  val B_LOC = 1
  val O_LOC = 2
  val UNKNOWN_TOKEN = "UNKNOWN_TOKEN"
  val TIME_DISTRIBUTED_1 = "time_distributed_1"
}

class GeoDisambiguateParser(modelPath: String, word2IdxPath: String, loc2geonameIDPath: String) {
  protected val network: ComputationGraph = KerasModelImport.importKerasModelAndWeights(modelPath, false)
  lazy protected val word2int: Map[String, Int] = readDict(word2IdxPath)
  // provide path of geoname dict file having geonameID with max population
  lazy protected val loc2geonameID: Map[String, Int] = readDict(loc2geonameIDPath)

  protected def readDict(dictPath: String): Map[String, Int] = {
    (Sourcer.sourceFromResource(dictPath)).autoClose { source =>
      source.getLines.map { line =>
        val words = line.split(' ')

        (words(0).toString -> words(1).toInt)
      }.toMap
    }
  }

  def makeFeatures(words: Array[String]): Array[Float] = {
    val unknown = word2int(GeoDisambiguateParser.UNKNOWN_TOKEN)
    val features = words.map(word2int.getOrElse(_, unknown).toFloat)

    features
  }

  def makeLabels(features: Array[Float]): Array[Int] = {

    def argmax(values: Array[Float]): Int = {
      // This goes through the values twice, but at least it doesn't create extra objects.
      val max = values.max

      values.indexWhere(_ == max)
    }

    val results = this.synchronized {
      val input = Nd4j.create(features)
      network.setInput(0, input)
      network.feedForward()
    }
    val output = results.get(GeoDisambiguateParser.TIME_DISTRIBUTED_1)
    val predictions: Array[Array[Float]] =
        if (output.shape()(0) == 1) Array(output.toFloatVector)
        else output.toFloatMatrix

    predictions.map(prediction => argmax(prediction))
  }

  def makeGeoLocations(labelIndexes: Array[Int], words: Array[String],
      startOffsets: Array[Int], endOffsets: Array[Int]): Seq[GeoPhraseID] = {

    def newGeoPhraseID(startIndex: Int, endIndex: Int): GeoPhraseID = {
      // The previous version changed a location phrase into which '_' had been
      // inserted into a prettyLocationPhrase by changing '_' to ' '.  However,
      // that is incorrect if the location phrase contained its own '_' characters.
      val prettyLocationPhrase = words.slice(startIndex, endIndex).mkString(" ")
      val locationPhrase = prettyLocationPhrase.replace(' ', '_').toLowerCase
      val geoNameId = loc2geonameID.get(locationPhrase)

      // The word at endIndex has ended previously, so use index - 1.
      GeoPhraseID(prettyLocationPhrase, geoNameId, startOffsets(startIndex), endOffsets(endIndex - 1))
    }

    val result = for {
      startIndex <- labelIndexes.indices
      labelIndex = labelIndexes(startIndex)
      isStart = labelIndex == GeoDisambiguateParser.B_LOC || ( // beginning of a location
          labelIndex == GeoDisambiguateParser.I_LOC && ( // inside of a location if
              startIndex == 0 || // labels start immediately on I_LOC
                  labelIndexes(startIndex - 1) == GeoDisambiguateParser.O_LOC // or without B_LOC or I_LOC preceeding
              )
          )
      if isStart
    }
    yield {
      // Either B_LOC or O_LOC, so !I_LOC, will start a new location and thus end the old one
      val endIndex = labelIndexes.indexWhere(_ != GeoDisambiguateParser.I_LOC, startIndex + 1)

      // If not found, endIndex == -1, then use one off the end
      newGeoPhraseID(startIndex, if (endIndex == -1) labelIndexes.size else endIndex)
    }
    result
  }
}
