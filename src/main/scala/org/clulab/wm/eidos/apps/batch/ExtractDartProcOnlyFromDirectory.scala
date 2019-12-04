package org.clulab.wm.eidos.apps.batch

import java.io.File

import org.clulab.processors.Document
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.attachments.LocationDocumentAttachment
import org.clulab.wm.eidos.document.attachments.TitleDocumentAttachment
import org.clulab.wm.eidos.groundings.EidosAdjectiveGrounder
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.utils.ThreadUtils
import org.clulab.wm.eidos.utils.Timer
import org.clulab.wm.eidos.utils.meta.DartMetaUtils
import org.clulab.serialization.json.DocOps
import org.clulab.wm.eidos.utils.StringUtils

import scala.collection.parallel.ForkJoinTaskSupport

object ExtractDartProcOnlyFromDirectory extends App {
  val inputDir = args(0)
  val outputDir = args(1)
  val timeFile = args(2)
  val threads = args(3).toInt

//  val doneDir = inputDir + "/done"
  val converter = DartMetaUtils.convertTextToMeta _

  val files = findFiles(inputDir, "txt")
  val parFiles = files.par

  Timer.time("Whole thing") {
    val timePrintWriter = FileUtils.printWriterFromFile(timeFile)
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

    val forkJoinPool = ThreadUtils.newForkJoinPool(threads)
    val forkJoinTaskSupport = new ForkJoinTaskSupport(forkJoinPool)
    parFiles.tasksupport = forkJoinTaskSupport

    parFiles.foreach { file =>
      try {
        // 1. Open corresponding output file
        println(s"Extracting from ${file.getName}")
        val timer = new Timer("Single file in parallel")
        val size = timer.time {
          // 2. Get the input file contents
          val text = FileUtils.getTextFromFile(file)
          // 3. Extract causal mentions from the text
          val document: Document = reader.annotate(text)
          // 4. Convert to JSON
          val json = document.jsonAST
          // Only do the processors part
          // 5. Write to output file
          val path = outputDir + "/" + StringUtils.beforeLast(file.getName, '.') + ".json"
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
          println(s"Exception for file $file")
          exception.printStackTrace()
      }
    }
    timePrintWriter.close()
  }
}
