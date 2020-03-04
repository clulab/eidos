package org.clulab.wm.eidos.apps

import java.io.File

import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.First
import org.clulab.wm.eidos.utils.Sourcer

object ConcatenateXsvs extends App {
  val inputDir = args(0)
  val outputFile = new File(args(1))

  FileUtils.printWriterFromFile(outputFile).autoClose { printWriter =>
    val first = First()

    FileUtils.findFiles(inputDir, ".tsv").foreach { file =>
      Sourcer.sourceFromFile(file).autoClose { source =>
        val drop = if (first.isTrue) 0 else 1

        source.getLines.drop(drop).foreach { line =>
          printWriter.print(line)
          printWriter.print("\n")
        }
      }
    }
  }
}
