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

  object Side extends Enumeration {
    type Side = Value
    val Subject, Object = Value
  }

  // Put used column names into variable, calculate index
  protected val columns = Seq(
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

  case class Row(fields: Array[String]) {

    def getGrounding(side: Side.Value): String =
        if (side == Side.Subject) fields(2)
        else fields(9)

    def setGrounding(side: Side.Value, grounding: String): Unit =
        if (side == Side.Subject) fields(2) = grounding
        else fields(9) = grounding

    def getGroundingScore(side: Side.Value): Double =
        if (side == Side.Subject) fields(3).toDouble
        else  fields(10).toDouble

    def setGroundingScore(side: Side.Value, value: Double): Unit =
        if (side == Side.Subject) fields(3) = value.toString
        else fields(10) = value.toString

    def getText(side: Side.Value): String =
        if (side == Side.Subject) fields(4)
        else fields(11)

    def setGroundingCorrect(side: Side.Value, correct: Boolean): Unit =
        if (side == Side.Subject) fields(7) = if (correct) "Y" else "N"
        else fields(14) = if (correct) "Y" else "N"

    def getCorrectGrounding(side: Side.Value): String =
        if (side == Side.Subject) fields(8)
        else fields(15)

    def getReader: String = fields(17)

    def getText: String = fields(18)
  }

  class ScoreHalf {
    var correct = 0
    var total = 0
    var possible = 0
    var skipped = 0

    def toString(caption: String): String =
        s"$caption: $correct / $total of $possible possible with $skipped eidos skipped"
  }

  class Scores {
    protected val subjScores = new ScoreHalf()
    protected val objScores = new ScoreHalf()

    protected def getSide(side: Side.Value): ScoreHalf =
        if (side == Side.Subject) subjScores
        else objScores

    def incPossible(side: Side.Value, condition: Boolean = true): Unit = if (condition) getSide(side).possible += 1

    def incSkipped(side: Side.Value, condition: Boolean = true): Unit = if (condition) getSide(side).skipped += 1

    def incCorrect(side: Side.Value, condition: Boolean = true): Unit = if (condition) getSide(side).correct += 1

    def incTotal(side: Side.Value, condition: Boolean = true): Unit = if (condition) getSide(side).total += 1

    override def toString: String = {
      subjScores.toString("Subject") + "\n" +
       objScores.toString(" Object")
    }
  }

  def findMatch(eidosMentions: Seq[EidosMention], subjText: String, objText: String, isEidos: Boolean): Option[(EidosMention, EidosMention)] = {
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

    if (foundMatchOpt.isEmpty && isEidos) {
      println(s"""Could not match subject "$subjText" and object "$objText" to any of these:""")
      eidosMentions.foreach { eidosMention =>
        val causesOpt = eidosMention.eidosArguments.get("cause")
        val effectsOpt = eidosMention.eidosArguments.get("effect")

        if (causesOpt.isDefined && effectsOpt.isDefined) {
          causesOpt.foreach { causes =>
            causes.foreach { cause =>
              println(s"cause: ${cause.odinMention.text}")
            }
          }
          effectsOpt.foreach { effects =>
            effects.foreach { effect =>
              println(s"effect: ${effect.odinMention.text}")
            }
          }
        }
      }
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

  protected def evaluateSide(side: Side.Value, row: Row, scores: Scores, caption: String, line: String, isEidos: Boolean,
      singleOntologyGroundingOpt: Option[OntologyAliases.SingleOntologyGrounding]): Unit = {
    val (actualName, value) = getNameAndValue(singleOntologyGroundingOpt)
    val expectedName = row.getCorrectGrounding(Side.Subject)
    val correct = expectedName == actualName

    if (actualName == "" && isEidos) {
      println(s"Can no longer ground $caption: $line")
      scores.incSkipped(side)
    }

    row.setGrounding(side, actualName)
    row.setGroundingScore(side, value)
    row.setGroundingCorrect(side, correct)

    scores.incCorrect(side, correct)
    scores.incTotal(side)
  }

  protected def evaluateRow(line: String, row: Row, scores: Scores, eidosSystem: EidosSystem, printWriter: PrintWriter,
      tsvWriter: TsvWriter): Boolean = {
    val isEidos = row.getReader == "eidos"
    val annotatedDocument = eidosSystem.extractFromText(row.getText)
    val eidosMentions = annotatedDocument.eidosMentions
    val subjAndObjMentionOpt = findMatch(eidosMentions, row.getText(Side.Subject), row.getText(Side.Object), isEidos)
    val subjAndObjSingleOntologyGroundingOpt = subjAndObjMentionOpt.map { case (subjMention, objMention) =>
      (topSingleOntologyGrounding(name, subjMention.grounding), topSingleOntologyGrounding(name, objMention.grounding))
    }

    // There is correct subj or obj in row, so test should be possible.
    // One of these must be true, since it is a valid row already.
    scores.incPossible(Side.Subject, row.getCorrectGrounding(Side.Subject).nonEmpty)
    scores.incPossible(Side.Object, row.getCorrectGrounding(Side.Object).nonEmpty)
    // However, the grounding does not produce a result.
    if (subjAndObjSingleOntologyGroundingOpt.isEmpty) {
      if (isEidos) {
        println("Can no longer ground eidos values for line: " + line)
        scores.incSkipped(Side.Subject, row.getCorrectGrounding(Side.Subject).nonEmpty)
        scores.incSkipped(Side.Object, row.getCorrectGrounding(Side.Object).nonEmpty)
      }
      false
    }
    else {
      val (subjSingleOntologyGroundingOpt, objSingleOntologyGroundingOpt) = subjAndObjSingleOntologyGroundingOpt.get

      evaluateSide(Side.Subject, row, scores, "subject", line, isEidos, subjSingleOntologyGroundingOpt)
      evaluateSide(Side.Object, row, scores, "object", line, isEidos, objSingleOntologyGroundingOpt)
      true
    }
  }

  val inputFile = args(0)
  val outputFile = args(1)
  val name = args(2)

  // Export from Excel isn't valid utf-8, but copy and paste is.
  // Procedure: Copy and paste out of excel.  Save as input document for this program.
  // Produce output document, load into editor, and copy back into Excel.
  Source.fromFile(new File(inputFile), "UTF-8").autoClose { source => // ISO-8859-1
    new PrintWriter(outputFile, "UTF-8").autoClose { printWriter =>
      lazy val eidosSystem = new EidosSystem()
      val tsvWriter = new TsvWriter(printWriter)
      val first = First()
      val scores = new Scores

      source.getLines.foreach { line =>
        val fields = TsvUtils.readln(line)

        if (first.isTrue) {
          val newLine = TsvUtils.stringln(fields: _*)

          if (line != newLine) throw new RuntimeException("The file doesn't seem to be formatted as expected.")
          else printWriter.println(line)
        }
        else {
          val row = Row(fields)
          val valid = row.getCorrectGrounding(Side.Subject).nonEmpty || row.getCorrectGrounding(Side.Object).nonEmpty

          if (!valid) printWriter.println(line)
          else
            if (evaluateRow(line, row, scores, eidosSystem, printWriter, tsvWriter)) tsvWriter.println(fields: _*)
            else printWriter.println(line)
        }
      }
      println(scores)
    }
  }
}
