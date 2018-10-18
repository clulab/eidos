package org.clulab.wm.eidos.graph

import org.clulab.odin.Mention
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.test.TestUtils

import scala.collection.Seq

class GraphTester(ieSystem: EidosSystem, text: String) {
  //val mentions = extractMentions(clean(text))
  val mentions = EidosMention.findReachableMentions(TestUtils.extractMentions(ieSystem, clean(text)))

  def getSpecialChars(s: String) = s.filter(c => c < 32 || 127 < c)

  def clean(messyText: String): String = {
    val cleanText = messyText
      .replace('|', ' ') // before trim so space will be trimmed if necessary
      .trim()
      .replace('\n', ' ')
      .replace('\r', ' ')
      .replace('\t', ' ')
      .replaceAll("  +", " ")
    val specialChars = getSpecialChars(cleanText)

    if (!specialChars.isEmpty())
      throw new IllegalArgumentException("Text contained a special chars: " + specialChars)
    cleanText
  }

  protected def toString(mentions: Seq[Mention]): String = {
    val stringBuilder = new StringBuilder()

    mentions.indices.foreach(index => stringBuilder.append(s"${index}: ${mentions(index).text}\n"))
    stringBuilder.toString()
  }

  protected def annotateTest(result: Seq[String]): Seq[String] =
    if (result == TestUtils.successful)
      result
    else
      result ++ Seq("Mentions:\n" + toString(mentions))

  def test(nodeSpec: NodeSpec): Seq[String] = annotateTest(nodeSpec.test(mentions, useTimeNorm))

  def test(edgeSpec: EdgeSpec): Seq[String] = annotateTest(edgeSpec.test(mentions, useTimeNorm))

  def useTimeNorm = ieSystem.timenorm.isDefined
}
