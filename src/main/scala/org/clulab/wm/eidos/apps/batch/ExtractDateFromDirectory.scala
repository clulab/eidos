package org.clulab.wm.eidos.apps.batch

import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.utils.MetaUtils

object ExtractDateFromDirectory extends App {
  val inputDir = args(0)
  val dateFile = args(1)

  FileUtils.printWriterFromFile(dateFile).autoClose { printWriter =>
    val files = findFiles(inputDir, "json")

    files.foreach { file =>
      try {
        val json = MetaUtils.getMetaData(file)
        val documentCreationTimes = MetaUtils.getDocumentCreationTimes(json)

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
