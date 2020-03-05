package org.clulab.wm.eidos.apps.batch

import java.io.File

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.EidosAdjectiveGrounder
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileEditor
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sourcer
import org.clulab.wm.eidos.utils.StringUtils
import org.clulab.wm.eidos.utils.ThreadUtils
import org.clulab.wm.eidos.utils.Timer
import org.clulab.wm.eidos.utils.meta.CluText
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable

object ExtractCluMetaFromDirectoryWithId extends App {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val inputDir = args(0)
  val metaDir = args(1)
  val outputDir = args(2)
  val timeFile = args(3)
  val mapFile = args(4)
  val threads = args(5).toInt

  val fileToIdMap = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    val fileToIdMap = new mutable.HashMap[String, String]()
    val bufferedSource = Sourcer.sourceFromFile(mapFile)
    bufferedSource.getLines().foreach { line =>
      val json = JsonMethods.parse(line)
      val filename = (json \ "file_name").extract[String]
      val id = (json \ "_id").extract[String]
      fileToIdMap += (filename -> id)
    }
    fileToIdMap
  }

  val doneDir = inputDir + "/done"
  val textToMeta = CluText.convertTextToMeta _

  val files = FileUtils.findFiles(inputDir, "txt")
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
    val options = EidosSystem.Options()
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
          val id = {
            // These all and with .txt
            val baseFilename = StringUtils.beforeLast(file.getName, '.', true)
            val extensions = Array(".html", ".htm", ".pdf")

            def getId(extension: String): Option[String] =
              fileToIdMap.get(baseFilename + extension)

            val extensionIndex = extensions.indexWhere { extension: String =>
              getId(extension).isDefined
            }
            val id = if (extensionIndex >= 0)
              getId(extensions(extensionIndex))
            else
              fileToIdMap.get(baseFilename)

            if (id.isEmpty)
              println("This shouldn't happen!")
            id.get
          }

          // 2. Get the input file text and metadata
          val metafile = textToMeta(file, metaDir)
          val eidosText = CluText(reader, file, Some(metafile))
          val text = eidosText.getText
          val metadata = eidosText.getMetadata
          // 3. Extract causal mentions from the text
          val annotatedDocuments = Seq(reader.extractFromText(text, options, metadata))
          // 4. Convert to JSON
          val corpus = new JLDCorpus(annotatedDocuments)
          val mentionsJSONLD = corpus.serialize(adjectiveGrounder)
          // 5. Write to output file
          val path = CluText.convertTextToJsonld(file, outputDir)
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
