package org.clulab.wm.eidos.apps.batch

import org.clulab.utils.ThreadUtils
import org.clulab.wm.eidos.{EidosOptions, EidosSystem}
import org.clulab.wm.eidos.serialization.jsonld.{JLDCorpus, JLDDeserializer}
import org.clulab.wm.eidos.utils.meta.CdrText
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.{FileEditor, FileUtils, Logging, Timer}

import java.io.{PrintWriter, StringWriter}

object ReExtractCdrMetaFromDirectory extends App with Logging {
  val inputDir = args(0)
  val outputDir = args(1)
  val timeFile = args(2)
  val threads = args(3).toInt

  val doneDir = inputDir + "/done"

  val files = FileUtils.findFiles(inputDir, "json")
  val parFiles = ThreadUtils.parallelize(files, threads)
  val options = EidosOptions()

  Timer.time("All files") {
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
            val eidosText = CdrText(file)
            val text = eidosText.getText
            val metadata = eidosText.getMetadata
            // 2. Extract causal mentions from the text
            val annotatedDocument = reader.extractFromText(text, options, metadata)
            // 3. Write to output file

            val jsonld1 = {
              val stringWriter = new StringWriter
              new PrintWriter(stringWriter).autoClose { stringWriter =>
                new JLDCorpus(annotatedDocument).serialize(stringWriter)
              }
              stringWriter.toString
            }

            val jsonld2 = {
              val json = jsonld1
              val corpus = deserializer.deserialize(json)
              val annotatedDocument = corpus.head
              annotatedDocument.allEidosMentions.foreach { eidosMention =>
                ontologyHandler.ground(eidosMention)
              }
              val stringWriter = new StringWriter
              new PrintWriter(stringWriter).autoClose { stringWriter =>
                new JLDCorpus(annotatedDocument).serialize(stringWriter)
              }
              stringWriter.toString
            }

            if (jsonld1 != jsonld2)
              println("Regrounding is not stable!")

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
