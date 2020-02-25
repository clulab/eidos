package org.clulab.wm.eidos.groundings

import java.time.ZonedDateTime

import org.clulab.utils.Serializer
import org.clulab.wm.eidos.SentencesExtractor
import org.clulab.wm.eidos.utils.Canonicalizer
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Namer
import org.clulab.wm.eidos.utils.Sourcer
import org.clulab.wm.eidos.utils.StringUtils
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.util.matching.Regex

class TableOntologyRow(val nodeName: String, example: Option[Array[String]] = None, pattern: Option[Array[Regex]] = None) {
  val values: Array[String] = null

  val patterns: Option[Array[Regex]] = null

  val namer: Namer = null
}

@SerialVersionUID(1000L)
class TableDomainOntology(val tableOntologyRows: Array[TableOntologyRow], override val version: Option[String], override val date: Option[ZonedDateTime]) extends DomainOntology with Serializable {

  def size: Integer = tableOntologyRows.length

  def getNamer(n: Integer): Namer = tableOntologyRows(n).namer

  def getValues(n: Integer): Array[String] = tableOntologyRows(n).values

  def getPatterns(n: Integer): Option[Array[Regex]] = tableOntologyRows(n).patterns

  def isLeaf(n: Integer): Boolean = false // How to get rid of this?

  def save(filename: String): Unit = {
    Serializer.save(this, filename)
  }
}

object TableDomainOntology {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  class TableDomainOntologyBuilder(sentenceExtractor: SentencesExtractor, canonicalizer: Canonicalizer, filter: Boolean) {

    protected def realFiltered(text: String): Seq[String] = {
      val result = sentenceExtractor.extractSentences(text).flatMap { sentence =>
        val lemmas: Array[String] = sentence.lemmas.get
        val tags: Array[String] = sentence.tags.get
        val ners: Array[String] = sentence.entities.get

        for {
          i <- lemmas.indices
          if canonicalizer.isCanonical(lemmas(i), tags(i), ners(i))
        } yield lemmas(i)
      }
      result // breakpoint
    }

    protected def fakeFiltered(text: String): Seq[String] = text.split(" +")

    protected val filtered: String => Seq[String] = if (filter) realFiltered else fakeFiltered

    def build(name: String, ontologyPath: String): TableDomainOntology = {
      val exampleFiles = FileUtils.findFiles(ontologyPath + "/examples", "txt").toSeq
      val exampleNames = exampleFiles.map { file => StringUtils.beforeLast(file.getName, '.') }
      if (exampleNames.distinct.size != exampleNames.size)
        println("Something is wrong")
      val patternFiles = FileUtils.findFiles(ontologyPath + "/patterns", "txt").toSeq
      val patternNames = patternFiles.map { file => StringUtils.beforeLast(file.getName, '_') }
      if (patternNames.distinct.size != patternNames.size) {
        println("Something is wrong")
        val repeats = patternNames.filter { patternName =>
          val count = patternNames.count { item => item == patternName }
          count > 1
        }
        println(repeats)
      }
      val allNodeNames = (exampleNames ++ patternNames).distinct
      val nodeMap = allNodeNames.map { nodeName =>
        val exampleFileOpt = exampleFiles.find { file => file.getName == nodeName + ".txt" }
        val patternFileOpt = patternFiles.find { file => file.getName == nodeName + "_pattern.txt" }

        nodeName -> (exampleFileOpt, patternFileOpt)
      }.toMap
      val emptyNodes = nodeMap.values.find { node => node._1.isEmpty || node._2.isEmpty }
      println(emptyNodes)
      val tableOntologyRows = nodeMap.map { case (nodeName, (exampleFileOpt, patternFileOpt)) =>
        val examples = exampleFileOpt.map { exampleFile =>
          Sourcer.sourceFromFile(exampleFile).autoClose {source =>
            // TODO: The path will be in the first line
            source.getLines.map  { example =>
              filtered(example)
            }.toArray
          }.flatten
        }
        val patterns = patternFileOpt.map { patternFile =>
          Sourcer.sourceFromFile(patternFile).autoClose {source =>
            source.getLines.map  { pattern =>
              s"(?i)$pattern".r
            }.toArray
          }
        }

        new TableOntologyRow(nodeName, examples, patterns)
      }.toArray
      new TableDomainOntology(tableOntologyRows, None, None)
    }
  }

  // Quick and dirty test
  def main(args: Array[String]): Unit = {
    new TableDomainOntology.TableDomainOntologyBuilder(null, null, false)
        .build("two_six", "../two_six")
  }
}
