package org.clulab.wm.eidos.apps

import java.io.File

import org.clulab.wm.eidos.groundings.TableDomainOntology
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.Counter
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sourcer
import org.clulab.wm.eidos.utils.StringUtils

object CullVectors extends App {
  val inVectorFile = new File(args(0))
  val inFrequencyFile = new File(args(1))
  val inOntologyDir = args(2)
  val outputFile = new File(args(3))
  val limit = args(4).toInt

  val substitutions = Seq(
    ("-lrb-", "("), ("-rrb-", ")"), // round
    ("-lsb-", "["), ("-rsb-", "]"), // square
    ("-lcb-", "{"), ("-rcb-", "}")  // curvy
  )
  val freqentWords = Sourcer.sourceFromFile(inFrequencyFile).autoClose { source =>
    val frequentWords = source
        .getLines
        .take(limit)
        .map(StringUtils.beforeFirst(_, '\t'))
        .map(_.toLowerCase) // We're punting here.  Lowercase will be compared to lowercase.
        .map { token =>
          substitutions.foldLeft(token) { case (token, (remove, insert)) =>
            token.replace(remove, insert)
          }
        }
        .toSet

    frequentWords
  }
  val reservedWords = {
    val tableDomainOntology = new TableDomainOntology
        .TableDomainOntologyBuilder(null, null, false)
        .build("two_six", "../two_six")
    val values = 0.until(tableDomainOntology.size)
        .flatMap(tableDomainOntology.getValues(_))
        .map(_.toLowerCase)
        .toSet

    values
  }
  val (goodLines, badLines) = Sourcer.sourceFromFile(inVectorFile).autoClose { source =>
    val goodAndBadLines = source
        .getLines()
        .drop(1)
        .toSeq
        .groupBy { line =>
          val word = StringUtils
              .beforeFirst(line, ' ')
              .toLowerCase

          reservedWords.contains(word) || freqentWords.contains(word)
        }

    (goodAndBadLines(true), goodAndBadLines(false))
  }
  val count = goodLines.size + 1
  val columns = goodLines.head.count(_ == ' ')
  val badLine = {
    val badFloats = new Array[Float](columns)

    badLines.foreach { line =>
      val values = line
          .split(' ')
          .drop(1)
          .map(_.toFloat)

      assert(values.length == columns)
      0.until(values.length)
          .foreach(index => badFloats(index) += values(index))
    }

    val badStrings = badFloats.map { badValue => (badValue / badLines.length).toString }
    val badLine = " " + badStrings.mkString(" ")

    badLine
  }

  // The \n is to force LF as eol even on Windows.
  FileUtils.printWriterFromFile(outputFile).autoClose { printWriter =>
    printWriter.print(count.toString + " " + columns)
    printWriter.print("\n")
    printWriter.print(badLine)
    printWriter.print("\n")
    goodLines.foreach { line =>
      printWriter.print(line)
      printWriter.print("\n")
    }
  }
}
