package org.clulab.wm.eidos.apps.batch

import java.io.File

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
import org.clulab.wm.eidos.utils.meta.DartEsMetaUtils
import org.clulab.wm.eidos.utils.meta.DartZipMetaUtils
import org.slf4j.Logger
import org.slf4j.LoggerFactory

object ExtractDartMetaFromDirectory extends App {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val inputDir = args(0)
  val outputDir = args(1)
  val timeFile = args(2)
  val threads = args(3).toInt

  val doneDir = inputDir + "/done"
  val converter = DartZipMetaUtils.convertTextToMeta _

  val files = findFiles(inputDir, "json")
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
          // 2. Get the input file contents
          val json = DartZipMetaUtils.getMetaData(file)
          val text = DartZipMetaUtils.getDartText(json).get
          val documentCreationTime = DartZipMetaUtils.getDocumentCreationTime(json)
          val documentId = DartZipMetaUtils.getDartDocumentId(json)
          val documentTitle = DartZipMetaUtils.getDartDocumentTitle(json)
          val documentLocation = DartZipMetaUtils.getDartDocumentLocation(json)
          // 3. Extract causal mentions from the text
          val annotatedDocuments = Seq(reader.extractFromTextWithDct(text, dct = documentCreationTime, id = documentId))
          documentTitle.foreach { documentTitle => TitleDocumentAttachment.setTitle(annotatedDocuments.head.document, documentTitle) }
          documentLocation.foreach { documentLocation => LocationDocumentAttachment.setLocation(annotatedDocuments.head.document, documentLocation) }
          // 4. Convert to JSON
          val corpus = new JLDCorpus(annotatedDocuments)
          val mentionsJSONLD = corpus.serialize(adjectiveGrounder)
          // 5. Write to output file
          val path = DartZipMetaUtils.convertTextToJsonld(outputDir, file)
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
          logger.error(s"Exception for file $file", exception)
      }
    }
    timePrintWriter.close()
  }
}
