package org.clulab.wm.eidos.apps.batch

import java.io.File
import java.util.concurrent.ForkJoinPool

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.utils.MetaUtils
import org.clulab.wm.eidos.utils.Sourcer
import org.clulab.wm.eidos.utils.StringUtils
import org.clulab.wm.eidos.utils.Timer
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods

import scala.collection.mutable
import scala.collection.parallel.ForkJoinTaskSupport

object ExtractMetaFromDirectoryWithId extends App {
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
  val converter = MetaUtils.convertTextToMeta _

  val files = findFiles(inputDir, "txt")
  val parFiles = files.par

  Timer.time("Whole thing") {
    val timePrintWriter = FileUtils.printWriterFromFile(timeFile)
    timePrintWriter.println("File\tSize\tTime")
    val timer = new Timer("Startup")

    timer.start()
    // Prime it first.  This counts on overall time, but should not be attributed
    // to any particular document.
    val reader = new EidosSystem()
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
          val id = {
            // These all and with .txt
            val baseFilename = StringUtils.beforeLast(file.getName(), '.', true)
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

            if (!id.isDefined)
              println("This shouldn't happen!")
            id.get
          }

          // 2. Get the input file contents
          val text = FileUtils.getTextFromFile(file)
          val json = MetaUtils.getMetaData(converter, metaDir, file)
          val documentCreationTime = MetaUtils.getDocumentCreationTime(json)
          val documentTitle = MetaUtils.getDocumentTitle(json)
          // 3. Extract causal mentions from the text
          val annotatedDocuments = Seq(reader.extractFromText(text, documentCreationTime = documentCreationTime))
          annotatedDocuments.head.document.id = Some(id)
          // 4. Convert to JSON
          val corpus = new JLDCorpus(annotatedDocuments)
          val mentionsJSONLD = corpus.serialize()
          // 5. Write to output file
          val path = MetaUtils.convertTextToJsonld(outputDir, file)
          FileUtils.printWriterFromFile(path).autoClose { pw =>
            pw.println(stringify(mentionsJSONLD, pretty = true))
          }
          // Now move the file to directory done
          val newPath = doneDir + "/" + file.getName
          file.renameTo(new File(newPath))

          text.size
        }
        this.synchronized {
          timePrintWriter.println(file.getName() + "\t" + size + "\t" + timer.elapsedTime.get)
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
