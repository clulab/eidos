package org.clulab.wm.eidos.groundings

import java.time.ZonedDateTime

import org.clulab.struct.Interval
import org.clulab.wm.eidos.SentencesExtractor
import org.clulab.wm.eidos.utils.Canonicalizer
import org.clulab.wm.eidos.utils.Namer

import scala.util.matching.Regex

trait DomainOntology {
  val version: Option[String] = None
  val date: Option[ZonedDateTime] = None

  def size: Integer

  def getNamer(n: Integer): Namer

  def getValues(n: Integer): Array[String]

  def getPatterns(n: Integer): Option[Array[Regex]]

  def isLeaf(n: Integer): Boolean

  def save(filename: String): Unit
}

object DomainOntology {
  val ESCAPE = "\\"
  val ESCAPED_ESCAPE = ESCAPE + ESCAPE
  val SEPARATOR = "/"
  val ESCAPED_SEPARATOR = ESCAPE + SEPARATOR

  def canonicalWordsFromSentence(sentencesExtractor: SentencesExtractor, canonicalizer: Canonicalizer,  text: String): Seq[String] = {
    for {
      s <- sentencesExtractor.extractSentences(text)
      canonicalWord <- canonicalizer.canonicalWordsFromSentence(s, Interval(0, s.words.length))
    } yield canonicalWord
  }
}