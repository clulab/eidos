package org.clulab.wm.eidos.apps

import java.io.File

import org.clulab.wm.eidos.groundings.Sanitizer
import org.clulab.wm.eidos.groundings.TableDomainOntology
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.Counter
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sourcer
import org.clulab.wm.eidos.utils.StringUtils

import scala.collection.mutable

object CullVectors extends App {
  val sanitize = true // Set this optionally
  val shortcut = true // and this as well.

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
  val inOntologyDir = args(2)
  val outputFile = new File(args(3))
  val limit = args(4).toInt

  // The words in gigaword have been lowercased and include these substitutions.
  val substitutions = Seq(
    ("-lrb-", "("), ("-rrb-", ")"), // round
    ("-lsb-", "["), ("-rsb-", "]"), // square
    ("-lcb-", "{"), ("-rcb-", "}")  // curvy
  )
  // This is Map[word, (index, freq)].  The index is used for separating frequent from infrequent words.
  // The freq is used to eventually weight the vectors for each word when
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
  val reservedWords = {
    val tableDomainOntology = new TableDomainOntology
        .TableDomainOntologyBuilder(null, null, false)
        .build("two_six", inOntologyDir)
    val values = tableDomainOntology
        .indices
        .flatMap(tableDomainOntology.getValues(_))
        // These are lowercased to match gigaword.
        .map(_.toLowerCase)
        .toSet

    values
  }
  // There must be a better way than to open the file twice.
  val columns = Sourcer.sourceFromFile(inVectorFile).autoClose { source =>
    val line = source.getLines.take(1).toSeq.head
    val Array(_, columns) =line.split(' ')

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
      val good = reservedWords.contains(word) || (0 <= index && index < limit)

      if (!good) { // Need to add to bad line
        val floats = StringUtils.afterFirst(line, ' ').split(' ').map(_.toFloat)

        addWeightedVec(badFloats, freq, floats)
      }
      good
    }.toVector
  }
  val betterLines = if (sanitize) {
    // This section is optional.  Use only if sanitation option is desired.
    val sanitizer = new Sanitizer()
    val sanitizedMap = {
      // This will be [sanitizedWord, (count, vector)]
      val sanitizedMap = new mutable.HashMap[String, (Int, Array[Float])]()

      goodLines.foreach { line =>
        val word = StringUtils.beforeFirst(line, ' ')
        val sanitizedWordOpt = sanitizer.sanitize(word)

        sanitizedWordOpt.map { sanitizedWord =>
          // If they are the same, then it will already be found.  We're just concerned with the others.
          val (count, array) = sanitizedMap.getOrElseUpdate(sanitizedWord, (0, new Array[Float](columns)))
          val floats = StringUtils.afterFirst(line, ' ').split(' ').map(_.toFloat)
          // It may have frequency zero if not in gigaword at all but included because of reserved words.  Make 1 if so.
          val freq = wordFrequencies.get(word.toLowerCase).map(_._2).getOrElse(1)

          addWeightedVec(array, freq, floats)
          sanitizedMap(sanitizedWord) = (count + 1, array)
        }
      }

      sanitizedMap
    }
    val sanitizedLines = sanitizedMap.map { case (sanitizedWord, (_, array)) =>
      val normalizedFloats = normalize(array)
      val normalizedStrings = normalizedFloats.map(_.toString)
      val normalizedLine = "\t" + sanitizedWord + " " + normalizedStrings.mkString(" ")

      normalizedLine
    }

    if (shortcut) {
      val shortcutLines = goodLines.filter { line =>
        val word = StringUtils.beforeFirst(line, ' ')

        // Only include the good line of its word is more than once in the sanitizedMap/moreLines.
        // If it is only there once, then the vectors are the same anyway, and so is the word.
        // It is therefore redundant.  getWord for SanitizedWord2Vec will find it in the sanitizedMap.
        val keep = sanitizedMap
            .get(word)
            .map(_._1 > 1)
            .getOrElse(true)

        keep
      }
      shortcutLines ++ sanitizedLines
    }
    else
      goodLines ++ sanitizedLines
  }
  else
    goodLines

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
