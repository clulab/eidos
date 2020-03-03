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

  // There are some punctuation in here such as -rrb- and -lrb-.
  // Can these be compared to words in the sentence?
  // They will not match with W2V though.

  val freqentWords = Sourcer.sourceFromFile(inFrequencyFile).autoClose { source =>
    source.getLines.take(limit).map { line =>
      StringUtils.beforeFirst(line, '\t')
    }.toSet
  }

  val reservedWords = {
    val tableDomainOntology = new TableDomainOntology.TableDomainOntologyBuilder(null, null, false)
        .build("two_six", "../two_six")
    val values = 0.until(tableDomainOntology.size).flatMap { index =>
      tableDomainOntology.getValues(index)
    }.toSet

    values
  }

  val (count, columns) = FileUtils.printWriterFromFile(outputFile).autoClose { printWriter =>
    Sourcer.sourceFromFile(inVectorFile).autoClose { source =>
      val lines = source.getLines()
      val counter = Counter()
      val columns = StringUtils.afterLast(lines.next, ' ').toInt

      lines.foreach { line =>
        val word = StringUtils.beforeFirst(line, ' ')

        if (reservedWords.contains(word) || freqentWords.contains(word) ||
            // Should reserved words be lowercased itself?  Assume they already are unless case is important.
            reservedWords.contains(word.toLowerCase) || freqentWords.contains(word.toLowerCase)) {
          counter.inc()
          printWriter.println(line)
        }
      }
      (counter.get, columns)
    }
  }

  println(count.toString + " " + columns) // Put this at the top of the file.
}
