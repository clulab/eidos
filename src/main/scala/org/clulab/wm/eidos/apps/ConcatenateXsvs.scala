package org.clulab.wm.eidos.apps

import java.io.File

import ai.lum.common.FileUtils._
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.{FileUtils, First, Sourcer}

object ConcatenateXsvs extends App {
  val inputDir = args(0)
  val outputFile = new File(args(1))
  val extension = outputFile.getExtension()

  FileUtils.printWriterFromFile(outputFile).autoClose { printWriter =>
    val first = First()

    FileUtils.findFiles(inputDir, extension).foreach { file =>
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
