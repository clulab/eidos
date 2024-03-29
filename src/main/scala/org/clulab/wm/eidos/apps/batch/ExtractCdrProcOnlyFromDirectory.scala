package org.clulab.wm.eidos.apps.batch

import org.clulab.processors.Document
import org.clulab.serialization.json.DocOps
import org.clulab.serialization.json.stringify
import org.clulab.utils.ThreadUtils
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.grounders.EidosAdjectiveGrounder
import org.clulab.wm.eidos.metadata.CdrText
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.Logging
import org.clulab.wm.eidoscommon.utils.Timer

object ExtractCdrProcOnlyFromDirectory extends App with Logging {
  val inputDir = args(0)
  val outputDir = args(1)
  val timeFile = args(2)
  val threads = args(3).toInt

//  val doneDir = inputDir + "/done"
  val files = FileUtils.findFiles(inputDir, "json")
  val parFiles = ThreadUtils.parallelize(files, threads)

  Timer.time("Whole thing") {
    val timePrintWriter = FileUtils.appendingPrintWriterFromFile(timeFile)
    timePrintWriter.println("File\tSize\tTime")
    val timer = new Timer("Startup")

    timer.start()
    // Prime it first.  This counts on overall time, but should not be attributed
    // to any particular document.
    val config = EidosSystem.defaultConfig
    val reader = new EidosSystem(config)
    // 0. Optionally include adjective grounding
    val adjectiveGrounder = EidosAdjectiveGrounder.fromEidosConfig(config)

    reader.extractFromText("This is a test.")
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
          // 3. Extract causal mentions from the text
          val document: Document = reader.annotate(text)
          // 4. Convert to JSON
          val json = document.jsonAST
          // Only do the processors part
          // 5. Write to output file
          val path = FileEditor(file).setDir(outputDir).setExt("json").get
          FileUtils.printWriterFromFile(path).autoClose { pw =>
            pw.println(stringify(json, pretty = true))
          }
          // Now move the file to directory done
//          val newPath = doneDir + "/" + file.getName
//          file.renameTo(new File(newPath))

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
