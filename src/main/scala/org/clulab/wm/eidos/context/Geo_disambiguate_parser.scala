package org.clulab.wm.eidos.context

import java.io.InputStream
import java.lang.NumberFormatException
import java.time.LocalDateTime

import com.codecommit.antixml.Elem
import org.clulab.anafora.Data
import org.clulab.timenorm.{TemporalCharbasedParser, TimeSpan}
import org.deeplearning4j.nn.graph.ComputationGraph
import org.deeplearning4j.nn.modelimport.keras.KerasModelImport
import org.nd4j.linalg.factory.Nd4j
import org.slf4j.{Logger, LoggerFactory}
import play.api.libs.json.Json

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Try


class Geo_disambiguate_parser (modelPath: String) {

  private val network: ComputationGraph = KerasModelImport.importKerasModelAndWeights(modelPath, false)
  lazy private val word2int = readDict(this.getClass.getResourceAsStream("/org/clulab/wm/eidos/english/context/word2idx_file.txt"))
  lazy private val int2label = Map(2->"O", 0->"I-LOC", 1->"B-LOC")
   val loc2geonameID = readDict(this.getClass.getResourceAsStream("/org/clulab/wm/eidos/english/context/geo_dict_with_population_SOUTH_SUDAN.txt")) // provide path of geoname dict file having geonameID with max population

  private def printModel(){
    println(this.network.summary())
  }


  private def readDict(dictFile: InputStream): collection.mutable.Map[String, Float] = {
    var word2Idx =  collection.mutable.Map("PADDING_TOKEN" -> 0.toFloat, "UNKNOWN_TOKEN" -> 1.toFloat)
    for (line <- scala.io.Source.fromInputStream(dictFile).getLines) {
      val words = line.split(" ")
        try {
          word2Idx += (words(0).toString -> words(1).toFloat)
        }
        catch {
          // case e: Exception => e.classOf[NumberFormatException]
          case e: Exception => None
          // println("we are getting this error")
        }

    }
    return word2Idx

  }

  /*
  private def readGeoname_ID(dictFile: InputStream): collection.mutable.Map[String, Float] = {
    var word2Idx =  collection.mutable.Map("PADDING_TOKEN" -> 0.toFloat, "UNKNOWN_TOKEN" -> 1.toFloat)
    for (line <- scala.io.Source.fromInputStream(InputStream).getLines) {
      val words = line.split(" ")
      word2Idx += (words(0).toString -> words(1).toFloat)
    }
    return word2Idx

  }
  */


  private def readDict_label(dictFile: InputStream): collection.mutable.Map[Double, String] = {
    var Idx2label =  collection.mutable.Map(100.toDouble -> "dummy") // initializing the word2idx
    for (line <- scala.io.Source.fromInputStream(dictFile).getLines) {
      val words = line.split(" ")
      Idx2label += ( words(1).toDouble -> words(0).toString)
    }
    return Idx2label

  }


  def parse(sourceText: String) {

    val words = create_word_input(sourceText)
    val word_labels = generate_NER_labels(words._1)
//    val phrases = get_complete_location_phrase(word_labels, words._2, loc2geonameID)  // I have commented because now I am also passing char start and end offset as parameters in EidosDocument.scala
//
//    // println("word labels are ", word_labels)
//    println("and phrases look like ", phrases)
//    phrases
  }

    def create_word_input(sourceText: String): (ListBuffer[Float], ListBuffer[String]) ={

    // println("the input sentence is ", sourceText)
    var word_feature = new ListBuffer[Float]
    var words_strings = new ListBuffer[String]

    for (word <- sourceText.split(" ")){
      words_strings += word.toString //we don't need to convert to string, but still keeping it here unless I test this code completely with Eidos.
      if (word2int.keySet.exists(_ == word.toString)){
        word_feature += word2int(word.toString)

      }
      else {
        word_feature += word2int("UNKNOWN_TOKEN")
      }

    }

    (word_feature, words_strings)

  }



  def generate_NER_labels(word_features: ListBuffer[Float]): ListBuffer[String] = {

    val word_input = Nd4j.create(word_features.toArray)
    this.network.setInput(0, word_input)
    val results = this.network.feedForward()

    val output = results.get("time_distributed_1")
    val label_predictions = output.shape()(0) match {
      case 1 => Array(results.get("time_distributed_1").toFloatVector())
      case _ => results.get("time_distributed_1").toFloatMatrix()
    }

    var predicted_labels = new ListBuffer[String]
    for (word1_label <- label_predictions) {

      predicted_labels += int2label(word1_label.zipWithIndex.maxBy(_._1)._2)
    }

     return predicted_labels
  }



//    def get_complete_location_phrase(word_labels: ListBuffer[String], words_text:ListBuffer[String], loc2geonameID:collection.mutable.Map[String, Float]): (ListBuffer[String], ListBuffer[String]) ={
def get_complete_location_phrase(word_labels: ListBuffer[String], words_text:ListBuffer[String],
                                 loc2geonameID:collection.mutable.Map[String, Float], Start_offset:Array[Int], End_offset: Array[Int]): ListBuffer[(String, String, Int, Int)] ={
    val label_w_index = word_labels zipWithIndex
    var prev_label = "dummy"
    var location_phrase = ""
    var start_phrase_char_offset = 0

    //var location_sent_phrases = new ListBuffer[String]
    //var location_GeoIDs = new ListBuffer[String]
    var locations = new ListBuffer[(String, String, Int, Int)]

    for (labels <- label_w_index){

      if (labels._1=="B-LOC"){
         if (location_phrase.length>1){
           //location_sent_phrases += location_phrase

           if (loc2geonameID.keySet.exists(_ == location_phrase.toLowerCase)){
             //location_GeoIDs += loc2geonameID(location_phrase).toString
             locations += ((location_phrase.replace('_', ' '), loc2geonameID(location_phrase.toLowerCase).toInt.toString, start_phrase_char_offset, End_offset(labels._2)))
           }
           else {
             //location_GeoIDs += "geoID_not_found"
             locations += ((location_phrase.replace('_', ' '), "geoID_not_found", start_phrase_char_offset, End_offset(labels._2)))
           }

         }
        location_phrase = (words_text(labels._2))  // initializing the location phrase with current word
        start_phrase_char_offset = Start_offset(labels._2)
        }

      else if (labels._1=="I-LOC") {
        if (location_phrase.length > 1) {
          location_phrase += ("_" + words_text(labels._2))
        }
        else {
          start_phrase_char_offset = Start_offset(labels._2)  // this case means that we are getting I-LOC but there was no B-LOC before this step.
          location_phrase = (words_text(labels._2))
        }
      }

      else if (labels._1 == "O") {
        if (location_phrase.length>1){
          //location_sent_phrases += location_phrase
          if (loc2geonameID.keySet.exists(_ == location_phrase.toLowerCase)){  // find the try except equivalent in scala
            //location_GeoIDs += loc2geonameID(location_phrase).toString
            locations += ((location_phrase.replace('_', ' '), loc2geonameID(location_phrase.toLowerCase).toInt.toString, start_phrase_char_offset, End_offset(labels._2)))
          }
          else {
            //location_GeoIDs += "geoID_not_found"
            locations += ((location_phrase.replace('_', ' '), "geoID_not_found", start_phrase_char_offset, End_offset(labels._2)))
          }
        }
        location_phrase = ""
      }


      prev_label = labels._1.toString  // we don't need prev label based on current logic of combining words from B- and I- tag.
    }
      //(location_sent_phrases, location_GeoIDs)
      locations
  }

}
