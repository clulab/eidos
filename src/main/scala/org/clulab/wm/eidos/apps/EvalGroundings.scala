package org.clulab.wm.eidos.apps
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.groundings.{OntologyGrounder, OntologyHandler}
import java.io.PrintWriter

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.struct.Interval
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.apps.GenerateGoldGroundingTSV.{ieSystem, lines, ontologyHandler}

// This app reads in a tsv file with gold groundings, regrounds the entities, and compares with the gold groundings.

object EvalGroundings extends App {

  // load tsv files from resources
  val originalFile = FileUtils.getTextFromFile("compositionalGrounderTestDoc/gold_groundings.tsv")
  val fileAsString: String = originalFile.toString
  val lines: Array[String] = fileAsString.split("\n")

  var outFilename = "compositionalGrounderTestDoc/groundingEval.tsv"
  val header =
      "Index\t" +
      "Sentence\t" +
      "Entity\t" +
      "Character Offsets\t" +
      "GOLD Flat Grounding\t" +
      "Flat Grounding\t" +
      "Flat Correct?\t" +
      "GOLD Concept Grounding\t" +
      "Concept Grounding\t" +
      "Concept Correct?\t" +
      "GOLD Process Grounding\t" +
      "Process Grounding\t" +
      "Process Correct?\t" +
      "GOLD Property Grounding\t" +
      "Property Grounding\t" +
      "Property Correct?"

  val pw = new PrintWriter(outFilename)
  pw.println(header)

//  val proc = new FastNLPProcessor()

  // make grounder to reground entities
  val ontologyHandler: OntologyHandler = new
      EidosSystem().components.ontologyHandler
  val ieSystem = new EidosSystem()

  // keep track of correct ones
  var evaluatedSentences = 0
  var flatGroundingsCorrect = 0
  var conceptGroundingsCorrect = 0
  var processGroundingsCorrect = 0
  var propertyGroundingsCorrect = 0

  // loop over each line in tsv file
  for (entry <- lines.tail) {
    val line = entry.split("\t")

    val annotated = line(0)
    val index = line(1)
    val sentence = line(2)
    val entity = line(3)
    val offsetsText = line(4)
    val offsets = offsetsText.slice(1, offsetsText.length()-1).split(",")
    val interval = Interval(offsets.head.toInt, offsets(1).toInt)
    val flatGroundingGOLD = line(5)
    val conceptGroundingGOLD = line(7)
    val processGroundingGOLD = line(9)
    val propertyGroundingGOLD = line(11)

    // only do the comparison if the gold entity has been annotated (e.g. made sure the gold grounding is correct)
    if (annotated == "y") {

      evaluatedSentences += 1

      // make a Document out of the sentence
      val document = ieSystem.annotate(sentence)

      // get all groundings for the entity
      val allGroundings = ontologyHandler.reground(sentence, interval, document)

      val flatGroundings = allGroundings("wm_flattened")
      val flatName = flatGroundings.headOption.get._1.name
      val flatScore = flatGroundings.headOption.get._2

      val conceptGroundings = allGroundings("wm_compositional/concept")
      val conceptName = if (conceptGroundings.headOption.isDefined) conceptGroundings.headOption.get._1.name else None
      val conceptScore = if (conceptGroundings.headOption.isDefined) conceptGroundings.headOption.get._2 else None

      val processGroundings = allGroundings("wm_compositional/process")
      val processName = if (processGroundings.headOption.isDefined) processGroundings.headOption.get._1.name else None
      val processScore = if (processGroundings.headOption.isDefined) processGroundings.headOption.get._2 else None

      val propertyGroundings = allGroundings("wm_compositional/property")
      val propertyName = if (propertyGroundings.headOption.isDefined) propertyGroundings.headOption.get._1.name else None
      val propertyScore = if (propertyGroundings.headOption.isDefined) propertyGroundings.headOption.get._2 else None

      // keep track of matches
      // TODO: make use of flatCorrect value to add to results spreadsheet
      if (flatName == flatGroundingGOLD) {
        flatGroundingsCorrect += 1
        val flatCorrect = true
      }
      else {
        val flatCorrect = false
      }

      if (conceptName.toString() == conceptGroundingGOLD) {
        conceptGroundingsCorrect += 1
        val conceptCorrect = true
      }
      else {
        val conceptCorrect = false
      }

      if (processName.toString() == processGroundingGOLD) {
        processGroundingsCorrect += 1
        val processCorrect = true
      }
      else {
        val processCorrect = false
      }
      if (propertyName.toString() == propertyGroundingGOLD) {
        propertyGroundingsCorrect += 1
        val propertyCorrect = true
      }
      else {
        val propertyCorrect = false
      }
//      val conceptCorrect = if (conceptName == conceptGroundingGOLD) true else false
//      val processCorrect = if (processName == processGroundingGOLD) true else false
//      val propertyCorrect = if (propertyName == propertyGroundingGOLD) true else false
    }



  }

  val flatAcc = flatGroundingsCorrect.toFloat/evaluatedSentences.toFloat
  val conceptAcc = conceptGroundingsCorrect.toFloat/evaluatedSentences.toFloat
  val processAcc = processGroundingsCorrect.toFloat/evaluatedSentences.toFloat
  val propertyAcc = propertyGroundingsCorrect.toFloat/evaluatedSentences.toFloat

  println(evaluatedSentences+" entities evaluated")
  println("flat grounding accuracy:\t"+flatAcc)
  println("concept grounding accuracy:\t"+conceptAcc)
  println("process grounding accuracy:\t"+processAcc)
  println("property grounding accuracy:\t"+propertyAcc)
}
