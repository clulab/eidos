package org.clulab.wm.eidos.context

import org.clulab.wm.eidos.utils.Sourcer
import org.deeplearning4j.nn.graph.ComputationGraph
import org.deeplearning4j.nn.modelimport.keras.KerasModelImport
import org.nd4j.linalg.factory.Nd4j

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Geo_disambiguate_parser {
  val INT2LABEL = Map(0 -> "I-LOC", 1 -> "B-LOC", 2 -> "O")
  val UNKNOWN_TOKEN = "UNKNOWN_TOKEN"
  val TIME_DISTRIBUTED_1 = "time_distributed_1"
}

class Geo_disambiguate_parser(modelPath: String, word2IdxPath: String, loc2geonameIDPath: String) {

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

  def create_word_input(words: Array[String]): Array[Float] = {
    val unknown = word2int(Geo_disambiguate_parser.UNKNOWN_TOKEN)
    val features = words.map(word2int.getOrElse(_, unknown).toFloat)

    features
  }

  def generate_NER_labels(word_features: Array[Float]): Array[String] = {
    val word_input = Nd4j.create(word_features.toArray)
    network.setInput(0, word_input)

    val results = network.feedForward()
    val output = results.get(Geo_disambiguate_parser.TIME_DISTRIBUTED_1)
    val label_predictions: Array[Array[Float]] = output.shape()(0) match {
      case 1 => Array(results.get(Geo_disambiguate_parser.TIME_DISTRIBUTED_1).toFloatVector())
      case _ => results.get(Geo_disambiguate_parser.TIME_DISTRIBUTED_1).toFloatMatrix()
    }

    label_predictions.map { word1_label =>
      Geo_disambiguate_parser.INT2LABEL(word1_label.zipWithIndex.maxBy(_._1)._2)
    }
  }

  def get_complete_location_phrase(word_labels: Array[String], words_text: Array[String],
      Start_offset: Array[Int], End_offset: Array[Int]): List[GeoPhraseID] = {

    var locations = new ListBuffer[GeoPhraseID]
    var location_phrase = ""
    var start_phrase_char_offset = 0

    def addLocation(index: Int) = {
      val prettyLocationPhrase = location_phrase.replace('_', ' ')
      val geoNameId = loc2geonameID.get(location_phrase.toLowerCase)

      locations += GeoPhraseID(prettyLocationPhrase, geoNameId, start_phrase_char_offset, End_offset(index))
    }

    for ((label, index) <- word_labels.zipWithIndex) {
      if (label == "B-LOC") {
        if (location_phrase.nonEmpty)
          addLocation(index)
        location_phrase = words_text(index)  // initializing the location phrase with current word
        start_phrase_char_offset = Start_offset(index)
      }
      else if (label == "I-LOC") {
        if (location_phrase.nonEmpty)
          location_phrase += ("_" + words_text(index))
        else {
          start_phrase_char_offset = Start_offset(index)  // this case means that we are getting I-LOC but there was no B-LOC before this step.
          location_phrase = words_text(index)
        }
      }
      else if (label == "O") {
        if (location_phrase.nonEmpty)
          addLocation(index)
        location_phrase = ""
      }
    }
    locations.toList
  }
}
