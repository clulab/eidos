package org.clulab.wm.wmexchanger2.apps

import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.Sourcer
import org.clulab.wm.eidoscommon.utils.StringUtils

import java.io.File
import java.nio.file.Files
import scala.collection.mutable

object CombineJobs extends App {
  val baseDir = "/E:/DocumentCollections/feb2022exp1-amy/input-all"
  val inputDirs = Array(
    baseDir + "/input-dones/done",
    baseDir + "/input-dones2/done",
    baseDir + "/input-dones4/done",
    baseDir + "/input-dones5/done",
    baseDir + "/input-dones6/done",
    baseDir + "/input-dones-not-sent/done",
    baseDir + "/input-undones3-bad/input",
    baseDir + "/input-undones3-bad/input/done"
  )
  val outputDir = baseDir + "/all"
  val extension = ".txt"

  inputDirs.foreach { inputDir =>
    require(new File(inputDir).exists)
  }
  FileUtils.ensureDirsExist(outputDir)

  val map = {
    val map = mutable.HashMap[String, mutable.Set[String]]()

    inputDirs.foreach { inputDir =>
      val inputFiles = FileUtils.findFiles(inputDir, extension)

      inputFiles.foreach { inputFile =>
        val documentId = StringUtils.beforeFirst(inputFile.getName, '+')

        Sourcer.sourceFromFile(inputFile).autoClose { source =>
          source.getLines().foreach { ontologyId =>
            val ontologyIds = map.getOrElseUpdate(documentId, mutable.Set[String]())

            ontologyIds += ontologyId
          }
        }
      }
    }
    map
  }

  map.foreach { case (documentId, ontologyIds) =>
    val outputFilename = outputDir + "/" + documentId + extension

    FileUtils.printWriterFromFile(outputFilename).autoClose { printWriter =>
      ontologyIds.toList.sorted.foreach { each =>
        printWriter.print(each)
        printWriter.print("\n")
      }
    }
  }
}
