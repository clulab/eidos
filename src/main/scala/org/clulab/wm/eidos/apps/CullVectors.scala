package org.clulab.wm.eidos.apps

import java.io.File

import org.clulab.wm.eidos.groundings.TableDomainOntology
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.Counter
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sourcer
import org.clulab.wm.eidos.utils.StringUtils

object CullVectors extends App {

  def l2(values: Array[Float]): Float =
      Math.sqrt(values.foldLeft(0f) { case (sum, value) => sum + value * value }).toFloat

  // Caution: This normalization happens in place.
  def normalize(values: Array[Float]): Array[Float] = {
    val length = l2(values)

    values.indices.foreach { index => values(index) /= length }
    values
  }

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
        // In this case, the others are simply ignored.  If their frequencies were to
        // be taken into account, that would have to change.
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
    val values = tableDomainOntology
        .indices
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
      val normalizedValues = normalize(values)

      assert(normalizedValues.length == badFloats.length)
      // Normalize them each before adding.
      normalizedValues
          .indices
          .foreach(index => badFloats(index) += normalizedValues(index))
    }

    // Although the vectors are not normalized to begin with, we'll normalize it now.
    // Word2Vec normalizes all incoming vectors.  Doing it twice will not hurt.
    val normalizedBadFloats = normalize(badFloats)
    val badStrings = normalizedBadFloats.map(_.toString)
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
