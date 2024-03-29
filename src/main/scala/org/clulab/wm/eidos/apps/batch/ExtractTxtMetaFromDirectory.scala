package org.clulab.wm.eidos.apps.batch

import org.clulab.utils.ThreadUtils
import org.clulab.wm.eidos.EidosOptions
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.metadata.PlainText
import org.clulab.wm.eidos.serialization.jsonld.JLDCorpus
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.Logging
import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.Timer

import java.io.File

object ExtractTxtMetaFromDirectory extends App with Logging {
  val inputDir = args(0)
  val outputDir = args(1)
  val timeFile = args(2)
  val threads = args(3).toInt

  val doneDir = inputDir + "/done"

  new File(doneDir).mkdirs()

  val files = FileUtils.findFiles(inputDir, "txt")
  val parFiles = ThreadUtils.parallelize(files, threads)
  val options = EidosOptions()

  Timer.time("All files") {
    val reader = Timer.time("Startup") {
      new EidosSystem()
    }

    FileUtils.appendingPrintWriterFromFile(timeFile).autoClose { timePrintWriter =>
      timePrintWriter.println("File\tSize\tTime")

      parFiles.foreach { file =>
        try {
          // 1. Open corresponding output file
          logger.info(s"Extracting from ${file.getName}")
          val timer = new Timer("Single file")
          val size = timer.time {
            // 1. Get the input file text and metadata
            val inputText = FileUtils.getTextFromFile(file)
            val eidosText = new PlainText(inputText)
            val text = eidosText.getText
            val metadata = eidosText.getMetadata
            // 2. Extract causal mentions from the text
            val annotatedDocument = reader.extractFromText(text, options, metadata)
            // 3. Write to output file
            val path = FileEditor(file).setDir(outputDir).setExt("jsonld").get
            FileUtils.printWriterFromFile(path).autoClose { printWriter =>
              new JLDCorpus(annotatedDocument).serialize(printWriter, regrounding = false)
            }
            // Now move the file to directory done
            val newFile = FileEditor(file).setDir(doneDir).get
            file.renameTo(newFile)

            text.length
          }
          this.synchronized {
            timePrintWriter.println(file.getName + "\t" + size + "\t" + timer.elapsedTime.get)
          }
        }
        catch {
          case exception: Exception =>
            logger.error(s"Exception for file $file", exception)
        }
      }
    }
  }
  Timer.summarize
}
