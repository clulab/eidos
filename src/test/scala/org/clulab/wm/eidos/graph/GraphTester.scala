package org.clulab.wm.eidos.graph

import org.clulab.odin.Mention
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.graph.TestResult.TestResults
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.test.TestUtils

import scala.collection.Seq

class GraphTester(ieSystem: EidosSystem, text: String) {
  //val mentions = extractMentions(clean(text))
  val mentions: Seq[Mention] = EidosMention.findReachableMentions(TestUtils.extractMentions(ieSystem, clean(text)))

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

    if (ieSystem.language == "english") {
      val specialChars = getSpecialChars(cleanText)
      if (!specialChars.isEmpty)
        throw new IllegalArgumentException("Text contained a special chars: " + specialChars)
    }
    cleanText
  }

  protected def toString(mentions: Seq[Mention]): String = {
    mentions.zipWithIndex.map{case (mention, index) => {
      s"$index: ${mention.text} ${mention.attachments.mkString(", ")}"
    }}.mkString("\n")
  }

  protected def annotateTest(result: Seq[String]): Seq[String] =
    if (result == TestUtils.successful)
      result
    else
      result ++ Seq("Mentions:\n" + toString(mentions))

  def test(nodeSpec: NodeSpec): Seq[String] = {
    val testResult = nodeSpec.test(mentions, useTimeNorm, useGeoNorm, testResults)

    annotateTest(testResult.complaints)
  }

  def test(edgeSpec: EdgeSpec): Seq[String] = {
    val testResult = edgeSpec.test(mentions, useTimeNorm, useGeoNorm, testResults)

    annotateTest(testResult.complaints)
  }

  def useTimeNorm: Boolean = ieSystem.loadableAttributes.timenorm.isDefined
  def useGeoNorm: Boolean = ieSystem.loadableAttributes.geonorm.isDefined
}
