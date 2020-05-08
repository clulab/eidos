package org.clulab.wm.eidos.apps

import java.io.File

import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.Counter
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sourcer
import org.clulab.wm.eidos.utils.StringUtils

object CullVectors extends App {

  def l2(values: Array[Float]): Float =
    math.sqrt(values.foldLeft(0f) { case (sum, value) => sum + value * value }).toFloat

  // Caution: This normalization happens in place.
  def normalize(values: Array[Float]): Array[Float] = {
    val length = l2(values)

    require(length > 0)
    values.indices.foreach { index => values(index) /= length }
    values
  }

  def addWeightedVec(dest: Array[Float], freq: Int, src: Array[Float]): Unit = {
    dest.indices.foreach { index =>
      dest(index) += src(index) * freq
    }
  }

  val inVectorFile = new File(args(0))
  val inFrequencyFile = new File(args(1))
  val outputFile = new File(args(2))
  val limit = args(3).toInt

  def keepByIndex(index: Int, freq: Int): Boolean = 0 <= index && index < limit

  def keepByFreq(index: Int, freq: Int): Boolean = 0 <= index && limit <= freq

  val keep = keepByFreq _

  // The words in gigaword have been lowercased and include these substitutions.
  val substitutions = Seq(
    ("-lrb-", "("), ("-rrb-", ")"), // round
    ("-lsb-", "["), ("-rsb-", "]"), // square
    ("-lcb-", "{"), ("-rcb-", "}")  // curvy
  )
  // This is Map[word, (index, freq)].  The index is used for separating frequent from infrequent words.
  // The freq is used to eventually weight the vectors for each word when words are combined into single vectors.
  val wordFrequencies: Map[String, (Int, Int)] = Sourcer.sourceFromFile(inFrequencyFile).autoClose { source =>
    val counter = Counter(-1)
    val frequentWords = source
        .getLines
        .map { line =>
          val Array(rawWord, freq) = line.split('\t')
          val cookedWord = substitutions.foldLeft(rawWord) { case (word, (remove, insert)) =>
            word.replace(remove, insert)
          }

          cookedWord -> (counter.inc(), freq.toInt)
        }.toMap

    frequentWords
  }
  // There must be a better way than to open the file twice.
  val columns = Sourcer.sourceFromFile(inVectorFile).autoClose { source =>
    val line = source.getLines.take(1).toSeq.head
    val Array(_, columns) = line.split(' ')

    columns.toInt
  }
  val badFloats = new Array[Float](columns)
  val goodLines = Sourcer.sourceFromFile(inVectorFile).autoClose { source =>
    source.getLines.drop(1).filter { line =>
      val word = StringUtils
          .beforeFirst(line, ' ')
          .toLowerCase
      val (index, freq) = wordFrequencies.getOrElse(word, (-1, 0))
      // 0 <= index implies wordFrequencies.contains(word).
      val good = keep(index, freq)

      if (!good) { // Need to add to bad line
        val floats = StringUtils.afterFirst(line, ' ').split(' ').map(_.toFloat)

        addWeightedVec(badFloats, freq, floats)
      }
      good
    }.toVector
  }
  // This skips over some options that are available in other versions of this program.
  val betterLines = goodLines

  val count = betterLines.size + 1
  // Although the vectors are not normalized to begin with, we'll normalize it now.
  // Word2Vec normalizes all incoming vectors.  Doing it twice will not hurt.
  val normalizedFloats = normalize(badFloats)
  val badStrings = normalizedFloats.map(_.toString)
  val badLine = " " + badStrings.mkString(" ")

  // The \n is to force LF as eol even on Windows.
  FileUtils.printWriterFromFile(outputFile).autoClose { printWriter =>
    printWriter.print(count.toString + " " + columns)
    printWriter.print("\n")
    printWriter.print(badLine)
    printWriter.print("\n")
    betterLines.foreach { line =>
      printWriter.print(line)
      printWriter.print("\n")
    }
  }
}
