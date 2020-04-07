package org.clulab.wm.eidos.graph

import org.clulab.odin.Mention
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.context.GeoNormFinder
import org.clulab.wm.eidos.graph.TestResult.TestResults
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.test.TestUtils

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

    if (ieSystem.components.language == "english") {
      val specialChars = getSpecialChars(cleanText)
      if (!specialChars.isEmpty)
        throw new IllegalArgumentException("Text contained a special chars: " + specialChars)
    }
    cleanText
  }

  protected def toString(mentions: Seq[Mention]): String = {
    mentions.zipWithIndex.map { case (mention, index) =>
      val id = mention.getClass.getSimpleName + "@" + System.identityHashCode(mention)

      s"#$index: ($id) = ${mention.text} ${mention.attachments.mkString(", ") }"
    }.mkString("\n\t")
  }

  protected def annotateTest(graphSpec: GraphSpec, result: Seq[String]): Seq[String] = {
    if (result == TestUtils.successful)
      result
    else Seq(
      Seq("\nErrors:\n"),
      Seq(result.mkString("\n\t")),
      Seq("\nMentions:\n"),
      Seq(toString(mentions)),
      Seq("\nFound:\n"),
      Seq(testResults.keySet.asScala.toSeq.map { graphSpec =>
        val testResult = testResults.get(graphSpec)
        val mentionOpt = testResult.mention
        val string = mentionOpt.map { mention =>
          val id = mention.getClass.getSimpleName + "@" + System.identityHashCode(mention)
          val index = mentions.indexOf(mention)

          s"$graphSpec: ($id) = [#$index], ${mention.text} ${mention.attachments.mkString(", ") }"
        }.getOrElse(s"$graphSpec: None")

        string
      }.mkString("\n\t"))
    ).flatten
  }

  def test(nodeSpec: NodeSpec): Seq[String] = {
    val testResult = nodeSpec.test(mentions, useTimeNorm, useGeoNorm, testResults)

    annotateTest(nodeSpec, testResult.complaints)
  }

  def test(edgeSpec: EdgeSpec): Seq[String] = {
    val testResult = edgeSpec.test(mentions, useTimeNorm, useGeoNorm, testResults)

    annotateTest(edgeSpec, testResult.complaints)
  }


  def test(humanMigrationEdgeSpec: HumanMigrationEdgeSpec): Seq[String] = {
    val testResult = humanMigrationEdgeSpec.test(mentions, useTimeNorm, useGeoNorm, testResults)

    annotateTest(humanMigrationEdgeSpec, testResult.complaints)
  }

  def useTimeNorm: Boolean = ieSystem.components.useTimeNorm
  def useGeoNorm: Boolean = ieSystem.components.useGeoNorm
}
