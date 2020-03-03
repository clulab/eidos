package org.clulab.wm.eidos.apps

import java.io.File

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

  // There are some punctuation in here

  val freqentWords = Sourcer.sourceFromFile(inFrequencyFile).autoClose { source =>
    source.getLines.take(limit).map { line =>
      StringUtils.beforeFirst(line, '\t')
    }.toSet
  }

  // Read these from the ontology?
  val reservedWords = Set("once", "upon", "a", "time")

  val (count, columns) = FileUtils.printWriterFromFile(outputFile).autoClose { printWriter =>
    Sourcer.sourceFromFile(inVectorFile).autoClose { source =>
      val lines = source.getLines()
      val counter = Counter()
      val columns = StringUtils.afterLast(lines.next, ' ').toInt

      lines.foreach { line =>
        val word = StringUtils.beforeFirst(line, ' ')

        if (reservedWords.contains(word) || freqentWords.contains(word) ||  freqentWords.contains(word.toLowerCase)) {
          counter.inc()
          printWriter.println(line)
        }
      }
      (counter.get, columns)
    }
  }

  println(count.toString + " " + columns) // Put this at the top of the file.
}
