package org.clulab.wm.eidos.apps

import java.io.File
import java.io.PrintWriter

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.OntologyAliases
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.First
import org.clulab.wm.eidos.utils.StringUtils
import org.clulab.wm.eidos.utils.TsvUtils
import org.clulab.wm.eidos.utils.TsvUtils.TsvWriter

import scala.io.Source

object EvalOntologyGrounders extends App {

  case class Row(fields: Array[String]) {
    def getSubjGrounding: String = fields(2)
    def setSubjGrounding(grounding: String): Unit = fields(2) = grounding

    def getSubjGroundingScore: Double = fields(3).toDouble
    def setSubjGroundingScore(value: Double): Unit = fields(3) = value.toString

    def getSubjText: String = fields(4)

    def setSubjGroundingCorrect(correct: Boolean): Unit = fields(7) = if (correct) "Y" else "N"

    def getCorrectSubjGrounding: String = fields(8)


    def getObjGrounding: String = fields(9)
    def setObjGrounding(grounding: String): Unit = fields(9) = grounding

    def getObjGroundingScore: Double = fields(10).toDouble
    def setObjGroundingScore(value: Double): Unit = fields(10) = value.toString

    def getObjText: String = fields(11)

    def setObjGroundingCorrect(correct: Boolean): Unit = fields(14) = if (correct) "Y" else "N"

    def getCorrectObjGrounding: String = fields(15)


    def getReader: String = fields(17)

    def getText: String = fields(18)
  }

  // Put used column names into variable, calculate index
  val columns = Seq(
    "IDX",                    //  0
    "UUID",                   //  1
    "SUBJ_GROUNDING",         //  2
    "SUBJ_GROUNDING_SCORE",   //  3
    "SUBJ_TEXT",              //  4
    "SUBJ_INTERVENTION",      //  5
    "SUBJ_MENTION_INCORRECT", //  6
    "SUBJ_GROUNDING_CORRECT", //  7
    "CORRECT_SUBJ_GROUNDING", //  8
    "OBJ_GROUNDING",          //  9
    "OBJ_GROUNDING_SCORE",    // 10
    "OBJ_TEXT",               // 11
    "OBJ_INTERVENTION",       // 12
    "OBJ_MENTION_INCORRECT",  // 13
    "OBJ_GROUNDING_CORRECT",  // 14
    "CORRECT_OBJ_GROUNDING",  // 15
    "CURATOR",                // 16
    "READER",                 // 17
    "TEXT"                    // 18
  )

  def findMatch(eidosMentions: Seq[EidosMention], subjText: String, objText: String): Option[(EidosMention, EidosMention)] = {
    val foundMatchOpt = eidosMentions.find { eidosMention =>
      val causesOpt = eidosMention.eidosArguments.get("cause")
      val effectsOpt = eidosMention.eidosArguments.get("effect")

      if (causesOpt.isDefined && effectsOpt.isDefined) {
        if (causesOpt.get.size > 1)
          println("There were multiple causes!")
        if (effectsOpt.get.size > 1)
          println("There were multiple effects!")

        val actualCauseText = causesOpt.get.head.odinMention.text
        val actualEffectText = effectsOpt.get.head.odinMention.text

        actualCauseText == subjText && actualEffectText == objText
      }
      else
        false
    }

    foundMatchOpt.map { foundMatch =>
      val cause = foundMatch.eidosArguments("cause").head
      val effect = foundMatch.eidosArguments("effect").head

      (cause, effect)
    }
  }

  protected def topSingleOntologyGrounding(name: String, groundings: OntologyAliases.OntologyGroundings): Option[OntologyAliases.SingleOntologyGrounding] = {
    val ontologyGroundingOpt = groundings.get(name)

    ontologyGroundingOpt.map { ontologyGrounding =>
      ontologyGrounding.grounding.maxBy { case (_, value) => value }
    }
  }

  protected def getNameAndValue(singleOntologyGrounderOpt: Option[OntologyAliases.SingleOntologyGrounding]): (String, Double) = {
    val nameAndValue = singleOntologyGrounderOpt.map { case (namer, value) =>
      val name = namer.name
      val shortName = StringUtils.afterFirst(name, '/')
      val prefix = "concept/causal_factor/"
      val shorterName =
        if (shortName.startsWith(prefix))
          shortName.substring(prefix.length)
        else
          shortName

      (shorterName, value.toDouble)
    }.getOrElse("", 0d)

    nameAndValue
  }

  val inputFile = args(0)
  val outputFile = args(1)
  val name = args(2)

  lazy val eidosSystem = new EidosSystem()
  val first = First()

  var subjCorrect = 0
  var subjTotal = 0
  var subjPossible = 0
  var subjSkipped = 0

  var objCorrect = 0
  var objTotal = 0
  var objPossible = 0
  var objSkipped = 0

  // Export from Excel isn't valid utf-8, but copy and paste is.
  // Procedure: Copy and paste out of excel.  Save as input document for this program.
  // Produce output document, load into editor, and copy back into Excel.
  Source.fromFile(new File(inputFile), "UTF-8").autoClose { source => // ISO-8859-1
    new PrintWriter(outputFile, "UTF-8").autoClose { printWriter =>
      val tsvWriter = new TsvWriter(printWriter)

      source.getLines.foreach { line =>
          println(line)
        if (first.isTrue) {
          val fields = TsvUtils.readln(line)
          val newLine = TsvUtils.stringln(fields: _*)

          if (line != newLine)
            throw new RuntimeException("The file doesn't seem to be formatted as expected.")
          else
            printWriter.println(line)
        }
        else {
          val fields = TsvUtils.readln(line)
          val row = Row(fields)
          val valid = row.getCorrectSubjGrounding.nonEmpty || row.getCorrectObjGrounding.nonEmpty

          if (!valid)
            printWriter.println(line)
          else {
            val annotatedDocument = eidosSystem.extractFromText(row.getText)
            val eidosMentions = annotatedDocument.eidosMentions
            val subjAndObjMentionOpt = findMatch(eidosMentions, row.getSubjText, row.getObjText)
            val subjAndObjSingleOntologyGroundingOpt = subjAndObjMentionOpt.map { case (subjMention, objMention) =>
              (topSingleOntologyGrounding(name, subjMention.grounding), topSingleOntologyGrounding(name, objMention.grounding))
            }

            if (row.getCorrectSubjGrounding.nonEmpty)
              subjPossible += 1
            if (row.getCorrectObjGrounding.nonEmpty)
              objPossible += 1

            if (subjAndObjSingleOntologyGroundingOpt.isEmpty) {
              if (row.getReader == "eidos") {
                println("Can no longer ground: " + line)
                if (row.getCorrectSubjGrounding.nonEmpty)
                  subjSkipped += 1
                if (row.getCorrectObjGrounding.nonEmpty)
                  objSkipped += 1
              }
              printWriter.println(line)
            }
            else {
              val subjAndObjSingleOntologyGrounding = subjAndObjSingleOntologyGroundingOpt.get
              val subjSingleOntologyGroundingOpt = subjAndObjSingleOntologyGrounding._1
              val objSingleOntologyGroundingOpt = subjAndObjSingleOntologyGrounding._2

              if (row.getCorrectSubjGrounding.nonEmpty) {
                val (actualName, value) = getNameAndValue(subjSingleOntologyGroundingOpt)
                val expectedName = row.getCorrectSubjGrounding
                val correct = expectedName == actualName

                if (actualName == "" && row.getReader == "eidos") {
                  println("Can no longer ground subject: " + line)
                  subjSkipped += 1
                }

                row.setSubjGrounding(actualName)
                row.setSubjGroundingScore(value)
                row.setSubjGroundingCorrect(correct)

                if (correct)
                  subjCorrect += 1
                subjTotal += 1
              }
              if (row.getCorrectObjGrounding.nonEmpty) {
                val (actualName, value) = getNameAndValue(objSingleOntologyGroundingOpt)
                val expectedName = row.getCorrectObjGrounding
                val correct = expectedName == actualName

                if (actualName == "" && row.getReader == "eidos") {
                  println("Can no longer ground object: " + line)
                  objSkipped += 1
                }

                row.setObjGrounding(actualName)
                row.setObjGroundingScore(value)
                row.setObjGroundingCorrect(correct)

                if (correct)
                  objCorrect += 1
                objTotal += 1
              }
              tsvWriter.println(fields: _*)
            }
          }
        }
      }
    }
  }
  println(s"Subject: $subjCorrect / $subjTotal of $subjPossible possible with $subjSkipped eidos skipped\n" +
          s" Object: $objCorrect / $objTotal of $objPossible possible with $objSkipped eidos skipped")
}
