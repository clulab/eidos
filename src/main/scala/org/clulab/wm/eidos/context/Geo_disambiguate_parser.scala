package org.clulab.wm.eidos.context

import org.clulab.wm.eidos.utils.{FileUtils, Sourcer}
import org.deeplearning4j.nn.graph.ComputationGraph
import org.deeplearning4j.nn.modelimport.keras.KerasModelImport
import org.nd4j.linalg.factory.Nd4j

import scala.collection.mutable.ListBuffer

object Geo_disambiguate_parser {
  val INT2LABEL = Map(0 -> "I-LOC", 1 -> "B-LOC", 2 -> "O")
  val PADDING_TOKEN = "PADDING_TOKEN"
  val UNKNOWN_TOKEN = "UNKNOWN_TOKEN"
}

class Geo_disambiguate_parser(modelPath: String, word2IdxPath: String = "/org/clulab/wm/eidos/english/context/word2idx_file.txt",
    loc2geonameIDPath: String = "/org/clulab/wm/eidos/english/context/geo_dict_with_population_SOUTH_SUDAN.txt") {

  protected val network: ComputationGraph = KerasModelImport.importKerasModelAndWeights(modelPath, false)

  lazy protected val word2int = readDict(word2IdxPath)
  lazy protected val loc2geonameID = readDict(loc2geonameIDPath) // provide path of geoname dict file having geonameID with max population

  protected def readDict(dictPath: String): collection.mutable.Map[String, Int] = {
    // These values are already in the file
    // Do they need to be read into the geonameID thing as well?
    val word2Idx = collection.mutable.Map(Geo_disambiguate_parser.PADDING_TOKEN -> 0, Geo_disambiguate_parser.UNKNOWN_TOKEN -> 1)
    val source = Sourcer.sourceFromResource(dictPath)

    for (line <- source.getLines) {
      val words = line.split(' ')

      word2Idx += (words(0).toString -> words(1).toInt)
    }
    source.close()
    return word2Idx
  }

  // Just get the words straight from Eidos
  def create_word_input(sourceText: String): (Seq[Float], Seq[String]) = {
    val unknown = word2int(Geo_disambiguate_parser.UNKNOWN_TOKEN)
    val words = sourceText.split(' ').toSeq
    val features = words.map(word2int.getOrElse(_, unknown).toFloat)

    (features, words)
  }

  def generate_NER_labels(word_features: ListBuffer[Float]): ListBuffer[String] = {
    val word_input = Nd4j.create(word_features.toArray)
    network.setInput(0, word_input)

    val results = network.feedForward()
    val output = results.get("time_distributed_1")
    val label_predictions = output.shape()(0) match {
      case 1 => Array(results.get("time_distributed_1").toFloatVector())
      case _ => results.get("time_distributed_1").toFloatMatrix()
    }

    // Map this
    val predicted_labels = new ListBuffer[String]
    for (word1_label <- label_predictions)
      predicted_labels += Geo_disambiguate_parser.INT2LABEL(word1_label.zipWithIndex.maxBy(_._1)._2)
    predicted_labels
  }

//    def get_complete_location_phrase(word_labels: ListBuffer[String], words_text:ListBuffer[String], loc2geonameID:collection.mutable.Map[String, Float]): (ListBuffer[String], ListBuffer[String]) ={
  def get_complete_location_phrase(word_labels: ListBuffer[String], words_text: ListBuffer[String],
      Start_offset: Array[Int], End_offset: Array[Int]): ListBuffer[(String, String, Int, Int)] = {

    var locations = new ListBuffer[(String, String, Int, Int)]
    var location_phrase = ""
    var start_phrase_char_offset = 0

    def addLocation(index: Int) = {
      val prettyLocationPhrase = location_phrase.replace('_', ' ')
      val geoNameId = loc2geonameID.get(location_phrase.toLowerCase).map(_.toInt.toString).getOrElse("geoID_not_found")

      locations += ((prettyLocationPhrase, geoNameId, start_phrase_char_offset, End_offset(index)))
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
    locations
  }
}
