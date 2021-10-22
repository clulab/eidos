package org.clulab.wm.ontologies

import java.time.ZonedDateTime

import org.clulab.struct.Interval
import org.clulab.wm.eidoscommon.Canonicalizer
import org.clulab.wm.eidoscommon.SentencesExtractor
import org.clulab.wm.eidoscommon.utils.Namer

import scala.util.matching.Regex

trait DomainOntologyNode {
  def getNamer: Namer // change to getName, getBranch
  def getValues: Array[String]

  def getPosValues: Array[String] = getValues

  def getNegValues: Array[String] = Array.empty

  def getPatterns: Option[Array[Regex]]
  def isLeaf: Boolean

  // If the Node is obvlious about its parents, for example if the information is not stored, then None.
  // If the Node knows of its parents, but there isn't one, because it's the root, then Some(None).
  // If The Node knows of its parents and it is not the root, then Some(Some(DomainOntologyNode)).
  def getParent: Option[Option[DomainOntologyNode]]
}

trait DomainOntology {
  def version: Option[String] = None
  def date: Option[ZonedDateTime] = None

  def nodes: IndexedSeq[DomainOntologyNode]
  def save(filename: String): Unit
}

object DomainOntology {
  val ESCAPE = "\\"
  val ESCAPED_ESCAPE: String = ESCAPE + ESCAPE
  val SEPARATOR = "/"
  val ESCAPED_SEPARATOR: String = ESCAPE + SEPARATOR

  // This takes care of possible /s in the node names so that paths can be built.
  def escaped(name: String): String = name
      .replace(ESCAPE, ESCAPED_ESCAPE)
      .replace(SEPARATOR, ESCAPED_SEPARATOR)

  // Sometimes the words in node names are concatenated with _.
  def unescaped(name: String): String = name
      .replace('_', ' ')

  // We're doing case insensitive matching.
  def toRegex(pattern: String): Regex = ("(?i)" + pattern).r

  def canonicalWordsFromSentence(sentencesExtractor: SentencesExtractor, canonicalizer: Canonicalizer,  text: String): Seq[String] = {
    for {
      s <- sentencesExtractor.extractSentences(text)
      canonicalWord <- canonicalizer.canonicalWordsFromSentence(s, Interval(0, s.words.length))
    } yield canonicalWord
  }
}

abstract class VersionedDomainOntology(
  override val version: Option[String],
  override val date: Option[ZonedDateTime]
) extends DomainOntology

trait IndexedDomainOntology {
  def getNamer(n: Integer): Namer
  def getValues(n: Integer): Array[String]
  def getPosValues(n: Integer): Array[String] = getValues(n)
  def getNegValues(n: Integer): Array[String] = Array.empty
  def getPatterns(n: Integer): Option[Array[Regex]]
  def isLeaf(n: Integer): Boolean
  def getParent(n: Integer): Option[Option[DomainOntologyNode]]
}

class IndexedDomainOntologyNode(indexedDomainOntology: IndexedDomainOntology, index: Int) extends DomainOntologyNode {

  override def getNamer: Namer = indexedDomainOntology.getNamer(index)

  override def getValues: Array[String] = indexedDomainOntology.getValues(index)

  override def getPatterns: Option[Array[Regex]] = indexedDomainOntology.getPatterns(index)

  override def isLeaf: Boolean = indexedDomainOntology.isLeaf(index)

  override def getParent: Option[Option[DomainOntologyNode]] = indexedDomainOntology.getParent(index)
}
