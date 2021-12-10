package org.clulab.wm.eidos.graph

import java.io.{PrintWriter, StringWriter}
import org.clulab.odin.Mention
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.graph.TestResult.TestResults
import org.clulab.wm.eidos.test.TestUtils
import org.clulab.wm.eidoscommon.Language

import scala.collection.JavaConverters._
import scala.collection.Seq
import scala.collection.mutable

class GraphTester(ieSystem: EidosSystem, text: String) {
  val annotatedDocument: AnnotatedDocument = ieSystem.extractFromText(clean(text), cagRelevantOnly = false)
  val odinMentions: Seq[Mention] = annotatedDocument.allOdinMentions.toVector // They are easier to debug than streams.
  val testResults = new  mutable.HashMap[GraphSpec, TestResult]

  def getSpecialChars(s: String): String = s.filter(c => c < 32 || 127 < c)

  def clean(messyText: String): String = {
    val cleanText = messyText
      .replace('|', ' ') // before trim so space will be trimmed if necessary
      .trim()
      .replace('\n', ' ')
      .replace('\r', ' ')
      .replace('\t', ' ')
      .replaceAll("  +", " ")

    if (ieSystem.components.languageOpt.get == Language.ENGLISH) {
      val specialChars = getSpecialChars(cleanText)
      if (specialChars.nonEmpty)
        throw new IllegalArgumentException("Text contained a special chars: " + specialChars)
    }
    cleanText
  }

  protected def mentionIds(mentions: Seq[Mention]): String = {
    val mentionIds = mentions.map(mentionId)
    val mentionsId =
        if (mentionIds.isEmpty) ""
        else mentionIds.mkString("(", ", ", ")")

    mentionsId
  }

  protected def mentionId(mention: Mention): String = {
    val id = s"${mention.getClass.getSimpleName}@${System.identityHashCode(mention)}[${mention.text}]"
    val argumentIds = mention.arguments.toSeq.map { case (key, values) =>
      s"$key = ${mentionIds(values)}"
    }
    val argumentsId =
        if (argumentIds.isEmpty) ""
        else argumentIds.mkString("(", ", ", ")")

    id + argumentsId
  }

  protected def annotateTest(graphSpec: GraphSpec, result: Seq[String]): Seq[String] = {
    if (result == TestUtils.successful)
      result
    else {
      val stringWriter = new StringWriter()
      val printWriter = new PrintWriter(stringWriter)

      printWriter.println()
      printWriter.println("Errors:")
      result.foreach { value =>
        printWriter.println("\t" + value)
      }
      printWriter.println("Mentions:")
      odinMentions.zipWithIndex.foreach { case (mention, index) =>
        printWriter.println(s"\t#$index: ${mentionId(mention)}")
      }
      printWriter.println("Found:")
      testResults.foreach { case (graphSpec, testResult) =>
        val mentionOpt = testResult.mention

        if (mentionOpt.isDefined) {
          val mention = mentionOpt.get
          val index = odinMentions.indexOf(mention)

          printWriter.println(s"\t$graphSpec = #$index: ${mentionId(mention)}")
        }
        else
          printWriter.println(s"\t$graphSpec = None")
      }
      printWriter.flush()

      val string = stringWriter.toString
      Seq(string)
    }
  }

  def test(nodeSpec: NodeSpec): Seq[String] = {
    val testResult = nodeSpec.test(odinMentions, useTimeNorm, useGeoNorm, testResults)

    annotateTest(nodeSpec, testResult.complaints)
  }

  def test(edgeSpec: EdgeSpec): Seq[String] = {
    val testResult = edgeSpec.test(odinMentions, useTimeNorm, useGeoNorm, testResults)

    annotateTest(edgeSpec, testResult.complaints)
  }

  def useTimeNorm: Boolean = ieSystem.components.useTimeNorm
  def useGeoNorm: Boolean = ieSystem.components.useGeoNorm
}
