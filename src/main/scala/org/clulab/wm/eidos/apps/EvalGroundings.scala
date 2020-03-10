package org.clulab.wm.eidos.apps
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.groundings.{OntologyGrounder, OntologyGrounding, OntologyHandler}
import java.io.PrintWriter

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.struct.Interval
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.apps.GenerateGoldGroundingTSV.{ieSystem, lines, ontologyHandler}
import org.clulab.wm.eidos.groundings.OntologyAliases.OntologyGroundings
import util.control.Breaks._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

// This app reads in a tsv file with gold groundings, regrounds the entities, and compares with the gold groundings.

object EvalGroundings extends App {

  // load tsv file of annotated gold groundings
  val originalFile = FileUtils.getTextFromFile("grounderEvalResources/gold_groundings_annotated.tsv")
  val fileAsString: String = originalFile.toString
  val lines: Array[String] = fileAsString.split("\n")

  var outFilename = "grounderEvalResources/evaluation_results.tsv"
  val header =
      "Index\t" +
      "Sentence\t" +
      "Entity\t" +
      "Character Offsets\t" +
      "cause/effect\t" +
      "GOLD Flat Grounding\t" +
      "Current Flat Grounding\t" +
      "Flat Correct?\t" +
      "GOLD Concept Grounding\t" +
      "Current Concept Grounding\t" +
      "Concept Correct?\t" +
      "GOLD Process Grounding\t" +
      "Current Process Grounding\t" +
      "Process Correct?\t" +
      "GOLD Property Grounding\t" +
      "Current Property Grounding\t" +
      "Property Correct?\t" +
      "Notes\t"

  val pw = new PrintWriter(outFilename)
  pw.println(header)

  // make grounder to reground entities
  val ontologyHandler: OntologyHandler = new
      EidosSystem().components.ontologyHandler
  val ieSystem = new EidosSystem()

  // keep track of correct groundings
  var evaluatedSentences = 0
  var flatGroundingsCorrect = 0f
  var conceptGroundingsCorrect = 0f
  var processGroundingsCorrect = 0f
  var propertyGroundingsCorrect = 0f

  // get top 5 groundings (as strings) to compare with gold groundings
  def getTop5(allGroundings: OntologyGroundings, grounder: String): ListBuffer[String] = {
    val groundings = allGroundings(grounder)
    var top5 = ListBuffer[String]()
    for (x <- groundings.take(5)) {
      top5 += x._1.name
    }
    if (top5.isEmpty) {
      top5 = ListBuffer("None")
    }
    top5
  }

  // loop over each line in tsv file (except header line)
  for (entry <- lines.tail) {
    val line = entry.split("\t")

    // cells from original tsv
    val annotated = line(0)
    val index = line(1)
    val sentence = line(2)
    val entity = line(3)
    val offsetsText = line(4)
    val offsets = offsetsText.slice(1, offsetsText.length()-1).split(",")
    val interval = Interval(offsets.head.toInt, offsets(1).toInt)
    val causeOrEffect = line(5)
    val flatGroundingGOLD = line(6).split(", ")
    val conceptGroundingGOLD = line(8).split(", ")
    val processGroundingGOLD = line(10).split(", ")
    val propertyGroundingGOLD = line(12).split(", ")

    // only do the comparison if the gold entity has been annotated (e.g. made sure the gold grounding is correct)
    if (annotated == "y") {

      evaluatedSentences += 1

      // make a Document out of the sentence
      val document = ieSystem.annotate(sentence)

      // get all groundings for the entity
      val allGroundings = ontologyHandler.reground(sentence, interval, document)

      // get top 5 groundings for each grounder/branch
      val top5flat      = getTop5(allGroundings, "wm_flattened")
      val top5concept   = getTop5(allGroundings, "wm_compositional/concept")
      val top5process   = getTop5(allGroundings, "wm_compositional/process")
      val top5property  = getTop5(allGroundings, "wm_compositional/property")

      var flatCorrect     = false
      var conceptCorrect  = false
      var processCorrect  = false
      var propertyCorrect = false

      var matchedFlat     = "None"
      var matchedConcept  = "None"
      var matchedProcess  = "None"
      var matchedProperty = "None"

//      println("Sentence:\t"+index)
//      println("top 5 groundings:")
//      println(top5flat)
//      println(top5concept)
//      println(top5process)
//      println(top5property)

      // keep track of matches, adding MRR score based on position in top 5
      breakable {
        for (i <- top5flat.indices) {
          if (flatGroundingGOLD.contains(top5flat(i))) {
            flatGroundingsCorrect += 1f/(top5flat.indexOf(top5flat(i))+1).toFloat
            flatCorrect = true
            matchedFlat = top5flat(i)
            break
          }
        }
      }
      breakable {
        for (i <- top5concept.indices) {
          if (conceptGroundingGOLD.contains(top5concept(i))) {
            conceptGroundingsCorrect += 1f/(top5concept.indexOf(top5concept(i))+1).toFloat
            conceptCorrect = true
            matchedConcept = top5concept(i)

            println("gold concept grounding:", conceptGroundingGOLD)
            println("\tmrr:", 1f/(top5concept.indexOf(top5concept(i))+1).toFloat)

            break
          }

        }
        println("gold concept grounding:", conceptGroundingGOLD)
        println("\tmrr:",0)
      }
      breakable {
        for (i <- top5process.indices) {
          if (processGroundingGOLD.contains(top5process(i))) {
            processGroundingsCorrect += 1f/(top5process.indexOf(top5process(i))+1).toFloat
            processCorrect = true
            matchedProcess = top5process(i)

            println("gold process grounding:", processGroundingGOLD)
            println("\tmrr:", 1f/(top5process.indexOf(top5process(i))+1).toFloat)

            break
          }
        }
        println("gold process grounding:", processGroundingGOLD)
        println("\tmrr:",0)
      }
      breakable {
        for (i <- top5property.indices) {
          if (propertyGroundingGOLD.contains(top5property(i))) {
            propertyGroundingsCorrect += 1f/(top5property.indexOf(top5property(i))+1).toFloat
            propertyCorrect = true
            matchedProperty = top5property(i)

            println("gold property grounding:", propertyGroundingGOLD)
            println("\tmrr:", 1f/(top5property.indexOf(top5property(i))+1).toFloat)

            break
          }
        }
        println("gold property grounding:", propertyGroundingGOLD)
        println("\tmrr:",0)
      }

      val returnedFlat = if (flatCorrect) matchedFlat else top5flat.mkString(", ")
      val returnedConcept = if (conceptCorrect) matchedConcept else top5concept.mkString(", ")
      val returnedProcess = if (processCorrect) matchedProcess else top5process.mkString(", ")
      val returnedProperty = if (propertyCorrect) matchedProperty else top5property.mkString(", ")

      val row =
        index + "\t" +
        sentence + "\t" +
        entity + "\t" +
        offsetsText + "\t" +
        causeOrEffect + "\t" +
        flatGroundingGOLD.mkString(", ") + "\t" +
        returnedFlat + "\t" +
        flatCorrect + "\t" +
        conceptGroundingGOLD.mkString(", ") + "\t" +
        returnedConcept + "\t" +
        conceptCorrect + "\t" +
        processGroundingGOLD.mkString(", ") + "\t" +
        returnedProcess + "\t" +
        processCorrect + "\t" +
        propertyGroundingGOLD.mkString(", ") + "\t" +
        returnedProperty + "\t" +
        propertyCorrect// + "\n"

      pw.println(row)
    }
  }

  val flatAcc = flatGroundingsCorrect/evaluatedSentences
  val conceptAcc = conceptGroundingsCorrect/evaluatedSentences
  val processAcc = processGroundingsCorrect/evaluatedSentences
  val propertyAcc = propertyGroundingsCorrect/evaluatedSentences

  println(evaluatedSentences+" entities evaluated")
  println("flat grounding accuracy:\t"+flatAcc)
  println("concept grounding accuracy:\t"+conceptAcc)
  println("process grounding accuracy:\t"+processAcc)
  println("property grounding accuracy:\t"+propertyAcc)

  pw.close()
}
