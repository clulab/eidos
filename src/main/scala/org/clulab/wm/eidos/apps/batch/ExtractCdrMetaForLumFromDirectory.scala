package org.clulab.wm.eidos.apps.batch

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.EidosAdjectiveGrounder
import org.clulab.wm.eidos.serialization.json.causeex.CausalAssertionDocument
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileEditor
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.ThreadUtils
import org.clulab.wm.eidos.utils.Timer
import org.clulab.wm.eidos.utils.meta.CdrText
import org.slf4j.Logger
import org.slf4j.LoggerFactory

object ExtractCdrMetaForLumFromDirectory extends App {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val inputDir = args(0)
  val outputDir = args(1)
  val timeFile = args(2)
  val threads = args(3).toInt

  val doneDir = inputDir + "/done"

  val files = FileUtils.findFiles(inputDir, "json")
  val parFiles = ThreadUtils.parallelize(files, threads)

  Timer.time("Whole thing") {
    val timePrintWriter = FileUtils.appendingPrintWriterFromFile(timeFile)
    timePrintWriter.println("File\tSize\tTime")
    val timer = new Timer("Startup")

    timer.start()
    // Prime it first.  This counts on overall time, but should not be attributed
    // to any particular document.
    val reader = new EidosSystem()
    val options = EidosSystem.Options()

    Timer.time("EidosPrimer") {
      reader.extractFromText("This is a test.")
    }
    timer.stop()
    timePrintWriter.println("Startup\t0\t" + timer.elapsedTime.get)

    parFiles.foreach { file =>
      try {
        // 1. Open corresponding output file
        logger.info(s"Extracting from ${file.getName}")
        val timer = new Timer("Single file in parallel")
        val size = timer.time {
          // 2. Get the input file text and metadata
          val eidosText = CdrText(file)
          val text = eidosText.getText
          val metadata = eidosText.getMetadata
          // 3. Extract causal mentions from the text
          val annotatedDocument = reader.extractFromText(text, options, metadata)
          // 4. Convert to JSON
          val corpus = new CausalAssertionDocument(annotatedDocument)
          val mentionsJSONLD = corpus.toJValue()
          // 5. Write to output file
          val path = FileEditor(file).setDir(outputDir).get
          FileUtils.printWriterFromFile(path).autoClose { pw =>
            pw.println(stringify(mentionsJSONLD, pretty = true))
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
    timePrintWriter.close()
  }
}
