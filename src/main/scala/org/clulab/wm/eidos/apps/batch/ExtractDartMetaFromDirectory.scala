package org.clulab.wm.eidos.apps.batch

import java.io.File
import java.util.concurrent.ForkJoinPool

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.apps.ExtractFromDirectory.config
import org.clulab.wm.eidos.document.attachments.LocationDocumentAttachment
import org.clulab.wm.eidos.document.attachments.TitleDocumentAttachment
import org.clulab.wm.eidos.groundings.EidosAdjectiveGrounder
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.utils.Timer
import org.clulab.wm.eidos.utils.meta.DartMetaUtils

import scala.collection.parallel.ForkJoinTaskSupport

object ExtractDartMetaFromDirectory extends App {
  val inputDir = args(0)
  val metaDir = args(1)
  val outputDir = args(2)
  val timeFile = args(3)
  val threads = args(4).toInt

  val doneDir = inputDir + "/done"
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

    val forkJoinPool = new ForkJoinPool(threads)
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
          val json = DartMetaUtils.getMetaData(converter, metaDir, file)
          val documentCreationTime = DartMetaUtils.getDocumentCreationTime(json)
          val documentId = DartMetaUtils.getDartDocumentId(json)
          val documentTitle = DartMetaUtils.getDartDocumentTitle(json)
          val documentLocation = DartMetaUtils.getDartDocumentLocation(json)
          // 3. Extract causal mentions from the text
          val annotatedDocuments = Seq(reader.extractFromTextWithDct(text, dct = documentCreationTime, id = documentId))
          documentTitle.foreach { documentTitle => TitleDocumentAttachment.setTitle(annotatedDocuments.head.document, documentTitle) }
          documentLocation.foreach { documentLocation => LocationDocumentAttachment.setLocation(annotatedDocuments.head.document, documentLocation) }
          // 4. Convert to JSON
          val corpus = new JLDCorpus(annotatedDocuments)
          val mentionsJSONLD = corpus.serialize(adjectiveGrounder)
          // 5. Write to output file
          val path = DartMetaUtils.convertTextToJsonld(outputDir, file)
          FileUtils.printWriterFromFile(path).autoClose { pw =>
            pw.println(stringify(mentionsJSONLD, pretty = true))
          }
          // Now move the file to directory done
          val newPath = doneDir + "/" + file.getName
          file.renameTo(new File(newPath))

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
