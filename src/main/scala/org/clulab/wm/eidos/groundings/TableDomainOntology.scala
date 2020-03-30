package org.clulab.wm.eidos.groundings

import java.io.File
import java.time.ZonedDateTime

import org.clulab.utils.Serializer
import org.clulab.wm.eidos.SentencesExtractor
import org.clulab.wm.eidos.utils.Canonicalizer
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.First
import org.clulab.wm.eidos.utils.Namer
import org.clulab.wm.eidos.utils.PassThruNamer
import org.clulab.wm.eidos.utils.Sourcer
import org.clulab.wm.eidos.utils.StringUtils
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.util.matching.Regex

@SerialVersionUID(1000L)
class TableOntologyRow(val path: String, val values: Option[Array[String]] = None, val patterns: Option[Array[Regex]] = None)

@SerialVersionUID(1000L)
class TableDomainOntology(val tableOntologyRows: Array[TableOntologyRow], override val version: Option[String], override val date: Option[ZonedDateTime]) extends DomainOntology with Serializable {

  def size: Integer = tableOntologyRows.length

  def getNamer(n: Integer): Namer = new PassThruNamer(tableOntologyRows(n).path)

  def getValues(n: Integer): Array[String] = tableOntologyRows(n).values.getOrElse(Array.empty)

  def getPatterns(n: Integer): Option[Array[Regex]] = tableOntologyRows(n).patterns

  def isLeaf(n: Integer): Boolean = true

  def save(filename: String): Unit = {
    Serializer.save(this, filename)
  }
}

object TableDomainOntology {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  // Quick and dirty test
  // Make some option here for different loader from resource, all one file

  def main(args: Array[String]): Unit = {
    val tableDomainOntology1 = new TableDomainOntologyBuilder(null, null, false)
        .buildFromFiles("two_six", "../two_six")

//    new TableDomainOntologyBuilder(null, null, false)
//        .convertFromFilesToResource("../two_six", "two_six.tbl")

    val tableDomainOntology2 = new TableDomainOntologyBuilder(null, null, false)
        .buildFromResource("/org/clulab/causeex/eidos/english/ontologies/two_six.tbl", None, None)

    println("Done")
  }
}

class TableDomainOntologyBuilder(sentenceExtractor: SentencesExtractor, canonicalizer: Canonicalizer, filter: Boolean) {

  protected def realFiltered(text: String): Seq[String] =
    DomainOntology.canonicalWordsFromSentence(sentenceExtractor, canonicalizer, text)

  protected def fakeFiltered(text: String): Seq[String] = text.split(" +")

  protected val filtered: String => Seq[String] = if (filter) realFiltered else fakeFiltered

  def sanityCheck(exampleFiles: Seq[File], patternFiles: Seq[File]): Unit = {
    val exampleNames = exampleFiles.map { file => StringUtils.beforeLast(file.getName, '.') }
    if (exampleNames.distinct.size != exampleNames.size)
      println("Something is wrong")

    val patternNames = patternFiles.map { file => StringUtils.beforeLast(file.getName, '_') }
    if (patternNames.distinct.size != patternNames.size) {
      println("Something is wrong")
      val repeats = patternNames.filter { patternName =>
        val count = patternNames.count { item => item == patternName }
        count > 1
      }
      println(repeats)
    }
  }

  def readFile(fileOpt: Option[File]): (Option[Array[String]], Option[Array[String]]) = {
    fileOpt.map { file =>
      Sourcer.sourceFromFile(file).autoClose { source =>
        val paths = {
          val lines = source.getLines.toArray
          val paths = lines
              .takeWhile { line => println(line); line.startsWith("# ") }
              .map { line => StringUtils.afterFirst(line, ' ') }

          assert(paths.length > 0)
          paths
        }
        val values = {
          val lines = source.reset.getLines.toArray
          val values = lines.drop(paths.size)
              .map { example => println(example); example }

          values
        }
        // Sometimes files contain especially a blank trailing line.
        val nonEmptyValues = values.filter(_.nonEmpty)

        (Some(paths), if (values.nonEmpty) Some(nonEmptyValues) else None)
      }
    }.getOrElse((None, None))
  }

  def buildFromFiles(name: String, ontologyPath: String): TableDomainOntology = {
    val exampleFiles = FileUtils.findFiles(ontologyPath + "/examples", "txt")
    val exampleNamesMap = exampleFiles.map { file =>
      StringUtils.beforeLast(file.getName, '.') -> file
    }.toMap
    val patternFiles = FileUtils.findFiles(ontologyPath + "/patterns", "txt")
    val patternNamesMap = patternFiles.map { file =>
      StringUtils.beforeLast(file.getName, '_') -> file
    }.toMap
    sanityCheck(exampleFiles, patternFiles)

    val allNodeNames = (exampleNamesMap.keys ++ patternNamesMap.keys).toSeq.distinct
    val tableOntologyRows = allNodeNames.flatMap { nodeName =>
      val exampleFileOpt = exampleNamesMap.get(nodeName)
      val patternFileOpt = patternNamesMap.get(nodeName)
      val (examplePathsOpt, exampleLinesOpt) = readFile(exampleFileOpt)
      val examplesOpt = exampleLinesOpt.map { lines => lines.flatMap(filtered) }
      val (patternPathsOpt, patternLinesOpt) = readFile(patternFileOpt)
      val patternsOpt = patternLinesOpt.map { lines => lines.map { line => s"(?i)$line".r } }

      if (examplePathsOpt.isDefined && patternPathsOpt.isDefined)
        if (examplePathsOpt.get.zip(patternPathsOpt.get).exists { case (left, right) => left != right })
          println("The paths don't match!")
      if (examplePathsOpt.isEmpty && patternPathsOpt.isEmpty)
        println("It shouldn't be here!")

      val paths = examplePathsOpt.getOrElse(patternPathsOpt.get)

      paths.map { path =>
        if (examplesOpt.isDefined && examplesOpt.get.contains(""))
          println("What happened?")
        new TableOntologyRow(path, examplesOpt, patternsOpt)
      }
    }.toArray
    new TableDomainOntology(tableOntologyRows, None, None)
  }

  def buildFromResource(resource: String, versionOpt: Option[String], dateOpt: Option[ZonedDateTime]): TableDomainOntology = {
    // A blank line splits the stanzas.
    val splitter: String => Boolean = line => !line.startsWith("# ")

    def splitLines(lines: List[String]): List[TableOntologyRow] = {
      val (stanza, rest) = lines.span( line => line != "")
      val (_, nameLines) = stanza.span(splitter)
      assert(nameLines.size >= 1)
      assert(nameLines.head == "# Name")
      val (names, pathLines) = nameLines.tail.span(splitter)
      assert(names.size == 1)
      assert(pathLines.size >= 1)
      assert(pathLines.head == "# Paths")
      val (paths, exampleLines) = pathLines.tail.span(splitter)
      assert(paths.size >= 1)
      assert(exampleLines.size >= 1)
      assert(exampleLines.head == "# Examples")
      val (examples, patternLines) = exampleLines.tail.span(splitter)
      assert(patternLines.size >= 1)
      assert(patternLines.head == "# Patterns")
      val (patterns, _) = patternLines.tail.span(splitter)
      val examplesOpt = if (examples.nonEmpty) Some(examples.toArray) else None
      val patternsOpt = if (patterns.nonEmpty) Some(patterns.map(pattern => s"(?i)$pattern".r).toArray) else None
      val rows = paths.map { path =>
        new TableOntologyRow(path, examplesOpt, patternsOpt)
      }

      rows ++ (if (rest.nonEmpty) splitLines(rest.tail) else Nil)
    }

    val tableDomainOntology = Sourcer.sourceFromResource(resource).autoClose { source =>
      val lines = source.getLines().toList
      val rows = splitLines(lines).toArray

      // TODO Need version and date for this one because in source code!
      new TableDomainOntology(rows, None, None)
    }

    tableDomainOntology
  }

  def convertFromFilesToResource(files: String, resource: String): Unit = {
    val exampleFiles = FileUtils.findFiles(files + "/examples", "txt")
    val exampleNamesMap = exampleFiles.map { file =>
      StringUtils.beforeLast(file.getName, '.') -> file
    }.toMap
    val patternFiles = FileUtils.findFiles(files + "/patterns", "txt")
    val patternNamesMap = patternFiles.map { file =>
      StringUtils.beforeLast(file.getName, '_') -> file
    }.toMap
    sanityCheck(exampleFiles, patternFiles)

    val allNodeNames = (exampleNamesMap.keys ++ patternNamesMap.keys).toSeq.distinct
    FileUtils.printWriterFromFile(resource).autoClose { printWriter =>
      val first = new First()

      allNodeNames.foreach { nodeName =>
        val exampleFileOpt = exampleNamesMap.get(nodeName)
        val patternFileOpt = patternNamesMap.get(nodeName)
        val (examplePathsOpt, exampleLinesOpt) = readFile(exampleFileOpt)
        val examplesOpt = exampleLinesOpt.map { lines => lines.flatMap(filtered) }
        val (patternPathsOpt, patternLinesOpt) = readFile(patternFileOpt)
        val patternsOpt = patternLinesOpt.map { lines => lines.map { line => s"(?i)$line".r } }

        if (examplePathsOpt.isDefined && patternPathsOpt.isDefined)
          if (examplePathsOpt.get.zip(patternPathsOpt.get).exists { case (left, right) => left != right })
            println("The paths don't match!")
        if (examplePathsOpt.isEmpty && patternPathsOpt.isEmpty)
          println("It shouldn't be here!")

        val paths = examplePathsOpt.getOrElse(patternPathsOpt.get)

        if (first.isFalse)
          printWriter.println()
        printWriter.println("# Name")
        printWriter.println(nodeName)
        printWriter.println("# Paths")
        paths.foreach(printWriter.println(_))
        printWriter.println("# Examples")
        examplesOpt.foreach { examples =>
          examples.foreach { example =>
            if (example == "")
              println("What happened?")
            else
              printWriter.println(example)
          }
        }
        printWriter.println("# Patterns")
        patternsOpt.foreach { patterns =>
          patterns.foreach { pattern =>
            if (pattern == "")
              println("What happened?")
            else
              printWriter.println(pattern)
          }
        }
      }
    }
  }
}

