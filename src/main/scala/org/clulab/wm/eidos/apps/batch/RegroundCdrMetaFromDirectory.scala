package org.clulab.wm.eidos.apps.batch

import org.clulab.utils.ThreadUtils
import org.clulab.wm.eidos.{EidosOptions, EidosSystem}
import org.clulab.wm.eidos.serialization.jsonld.{JLDCorpus, JLDDeserializer}
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.{FileEditor, FileUtils, Logging, Timer}

object RegroundCdrMetaFromDirectory extends App with Logging {

  def run(args: Array[String]): Unit = {
    val inputDir = args(0) // These will be jsonld files that were already ready.
    val outputDir = args(1)
    val timeFile = args(2)
    val threads = args(3).toInt

    val doneDir = inputDir + "/done"

    val files = FileUtils.findFiles(inputDir, "jsonld")
    val parFiles = ThreadUtils.parallelize(files, threads)
    val options = EidosOptions()

    val reader = Timer.time("Startup") {
      new EidosSystem()
    }
    val ontologyHandler = reader.components.ontologyHandlerOpt.get
    val deserializer = new JLDDeserializer()


    FileUtils.appendingPrintWriterFromFile(timeFile).autoClose { timePrintWriter =>
      timePrintWriter.println("File\tSize\tTime")

      parFiles.foreach { file =>
        try {
          // 1. Open corresponding output file
          logger.info(s"Extracting from ${file.getName}")
          val timer = new Timer("Single file")
          val size = timer.time {
            // 1. Get the input file text and metadata
            val json = FileUtils.getTextFromFile(file)
            val corpus = deserializer.deserialize(json)
            //val metadata = eidosText.getMetadata
            // 2. Extract causal mentions from the text
            val annotatedDocument = corpus.head
            annotatedDocument.allEidosMentions.foreach { eidosMention =>
              ontologyHandler.ground(eidosMention)
            }
            // 3. Write to output file
            val path = FileEditor(file).setDir(outputDir).setExt("jsonld").get
            FileUtils.printWriterFromFile(path).autoClose { printWriter =>
              new JLDCorpus(annotatedDocument).serialize(printWriter)
            }
            // Now move the file to directory done
            val newFile = FileEditor(file).setDir(doneDir).get
            file.renameTo(newFile)

            annotatedDocument.document.text.get.length
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

  Timer.time("All files") {
    run (args)
  }
  Timer.summarize
}
