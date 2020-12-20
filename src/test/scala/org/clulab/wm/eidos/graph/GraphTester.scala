package org.clulab.wm.eidos.graph

import java.io.{PrintWriter, StringWriter}
import org.clulab.odin.Mention
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments.EidosAttachment
import org.clulab.wm.eidos.graph.TestResult.TestResults
import org.clulab.wm.eidos.serialization.json.JsonUtils
import org.clulab.wm.eidos.test.TestUtils
import org.clulab.wm.eidoscommon.Language

import scala.collection.JavaConverters._
import scala.collection.Seq

class GraphTester(ieSystem: EidosSystem, text: String) {
  val annotatedDocument = ieSystem.extractFromText(clean(text), cagRelevantOnly = false)
  val mentions: Seq[Mention] = annotatedDocument.allOdinMentions
  val testResults = new TestResults()

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
      if (!specialChars.isEmpty)
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
    val attachmentIds = mention.attachments.map { attachment =>
      val eidosAttachment = attachment.asInstanceOf[EidosAttachment]
      val text = JsonUtils.stringify(eidosAttachment.toJson, false)
      s"+${eidosAttachment.getClass.getSimpleName}@${System.identityHashCode(eidosAttachment)}($text)"
    }
    val attachmentsId =
      if (attachmentIds.isEmpty) ""
      else attachmentIds.mkString("")
    val id = s"[{${mention.getClass.getSimpleName}@${System.identityHashCode(mention)}}${mention.text}" +
        {
          if (attachmentsId.isEmpty) ""
          else "|" + attachmentsId
        } +
        "]"
    val argumentIds = mention.arguments.toSeq.map { case (key, values) =>
      s"$key = ${mentionIds(values)}"
    }
    val argumentsId =
        if (argumentIds.isEmpty) ""
        else argumentIds.mkString("(", ", ", ")")

    id + attachmentsId + argumentsId
  }

  protected def annotateTest(graphSpec: GraphSpec, result: Seq[String]): Seq[String] = {
    if (result == TestUtils.successful)
      result
    else {
      val stringWriter = new StringWriter()
      val printWriter = new PrintWriter(stringWriter)

      printWriter.println
      printWriter.println("Errors:")
      result.foreach { value =>
        printWriter.println("\t" + value)
      }
      printWriter.println("Mentions:")
      mentions.zipWithIndex.foreach { case (mention, index) =>
        printWriter.println(s"\t#$index: ${mentionId(mention)}")
      }
      printWriter.println("Found:")
      testResults.keySet.asScala.toSeq.foreach { graphSpec =>
        val testResult = testResults.get(graphSpec)
        val mentionOpt = testResult.mention

        if (mentionOpt.isDefined) {
          val mention = mentionOpt.get
          val index = mentions.indexOf(mention)

          printWriter.println(s"\t$graphSpec = #$index: ${mentionId(mention)}")
        }
        else
          printWriter.println(s"\t$graphSpec = None")
      }
      printWriter.flush

      val string = stringWriter.toString
      Seq(string)
    }
  }

  def test(nodeSpec: NodeSpec): Seq[String] = {
    val testResult = nodeSpec.test(mentions, useTimeNorm, useGeoNorm, testResults)

    annotateTest(nodeSpec, testResult.complaints)
  }

  def test(edgeSpec: EdgeSpec): Seq[String] = {
    val testResult = edgeSpec.test(mentions, useTimeNorm, useGeoNorm, testResults)

    annotateTest(edgeSpec, testResult.complaints)
  }

  def useTimeNorm: Boolean = ieSystem.components.useTimeNorm
  def useGeoNorm: Boolean = ieSystem.components.useGeoNorm
}
