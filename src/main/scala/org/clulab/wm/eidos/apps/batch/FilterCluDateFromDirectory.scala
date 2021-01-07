package org.clulab.wm.eidos.apps.batch

import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidos.utils.meta.CluText

object FilterCluDateFromDirectory extends App {
  val inputDir = args(0)
  val dateFile = args(1)

  FileUtils.printWriterFromFile(dateFile).autoClose { printWriter =>
    val files = FileUtils.findFiles(inputDir, "json")

    files.foreach { file =>
      try {
        val documentCreationTimes = CluText.getDocumentCreationTimes(file)
        // Best date
//        if (documentCreationTimes.nonEmpty)
//          printWriter.println(documentCreationTimes(0))
        // Add dates
        documentCreationTimes.foreach(printWriter.println)
        printWriter.println
      }
      catch {
        case exception: Exception =>
          println(s"Exception for file $file")
          exception.printStackTrace()
      }
    }
  }
}
