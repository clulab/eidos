package org.clulab.wm.wmexchanger2.wmeidos

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.clulab.wm.eidos.EidosOptions
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.metadata.CdrText
import org.clulab.wm.eidos.serialization.jsonld.JLDCorpus
import org.clulab.wm.eidos.serialization.jsonld.JLDDeserializer
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.Counter
import org.clulab.wm.eidoscommon.utils.Sourcer
import org.clulab.wm.eidoscommon.utils.{FileEditor, FileUtils, LockUtils, StringUtils}
import org.clulab.wm.wmexchanger.utils.DevtimeConfig
import org.clulab.wm.wmexchanger.utils.Extensions
import org.clulab.wm.wmexchanger.utils.LoopApp
import org.clulab.wm.wmexchanger.utils.SafeThread
import org.clulab.wm.wmexchanger2.utils.AppEnvironment
import org.clulab.wm.wmexchanger2.utils.FileName
import org.clulab.wm.wmexchanger2.utils.OntologyMap
import org.clulab.wm.wmexchanger2.utils.Stages

import java.io.File
import java.nio.file.Files
import java.util.concurrent.Executors
import scala.collection.mutable.{HashSet => MutableHashSet}

class EidosLoopApp2(inputDir: String, outputDir: String, doneDir: String,
                    documentDir: String, ontologyDir: String, readingDir: String, threads: Int) {
  var useReal = EidosLoopApp2.useReal

  val config: Config = ConfigFactory.defaultApplication().resolve().getConfig("eidos")
  val interactive: Boolean = config.getBoolean("interactive")
  val waitDuration: Int = config.getInt("duration.wait")
  val pauseDuration: Int = config.getInt("duration.pause")

  val options: EidosOptions = EidosOptions()
  val reader =
      if (useReal) new RealEidosSystem()
      else new MockEidosSystem(System.getenv("MOCK_DIR"))
  val deserializer = new JLDDeserializer()
  val ontologyMap = new OntologyMap(reader, ontologyDir)

  def ground(ontologyId: String, annotatedDocument: AnnotatedDocument): AnnotatedDocument = {
    val ontologyHandler = ontologyMap(ontologyId)

    annotatedDocument.allEidosMentions.foreach(ontologyHandler.ground)
    annotatedDocument
  }

  def processGroundingFile(file: File, filesBeingProcessed: MutableHashSet[String], outputDistinguisher: Counter, doneDistinguisher: Counter): Unit = {
    try {
      val fileName = FileName(file)
      val documentId = fileName.getDocumentId
      val ontologyIds = Sourcer.sourceFromFile(file).autoClose { source =>
        source.getLines.toVector
      }
      val readingFile = FileEditor(new File(documentId)).setExt(Extensions.jsonld).setDir(readingDir).get
      val annotatedDocument =
          try {
            val json = FileUtils.getTextFromFile(readingFile)

            deserializer.deserialize(json).head
          }
          catch {
            case exception: Throwable =>
              EidosLoopApp2.logger.error(s"Exception for file $file", exception)
              reader.getEmptyAnnotatedDocument(Some(documentId))
          }

      ontologyIds.foreach { ontologyId =>
        val outputFile = fileName.addExt(Extensions.jsonld).setName(1, ontologyId).distinguish(EidosLoopApp2.outputStage, outputDistinguisher).setDir(outputDir).toFile

        ground(ontologyId, annotatedDocument)

        LockUtils.withLock(readingFile, Extensions.lock) {
          FileUtils.printWriterFromFile(outputFile).autoClose { printWriter =>
            new JLDCorpus(annotatedDocument).serialize(printWriter)
          }
        }
      }

      val doneFile = FileName(file).distinguish(EidosLoopApp2.outputStage, doneDistinguisher).setDir(doneDir).toFile
      FileUtils.rename(file, doneFile)
    }
    catch {
      case exception: Exception =>
        EidosLoopApp2.logger.error(s"Exception for file $file", exception)
    }
  }

  def processReadingFile(file: File, filesBeingProcessed: MutableHashSet[String], outputDistinguisher: Counter, doneDistinguisher: Counter): Unit = {
    try {
      val fileName = FileName(file)
      val documentId = fileName.getDocumentId
      val ontologyIds = Sourcer.sourceFromFile(file).autoClose { source =>
        source.getLines.toVector
      }
      val documentFile = FileEditor(new File(documentId)).setExt(Extensions.json).setDir(documentDir).get
      val annotatedDocument =
          try {
            val eidosText = CdrText(documentFile)
            val text = eidosText.getText
            val metadata = eidosText.getMetadata

            reader.extractFromText(text, options, metadata)
          }
          catch {
            case exception: Throwable =>
              EidosLoopApp2.logger.error(s"Exception for file $file", exception)
              reader.getEmptyAnnotatedDocument(Some(documentId))
          }
      val readingFile = FileEditor(new File(documentId)).setExt(Extensions.jsonld).setDir(readingDir).get

      // The same file could be written multiple times, depending on the jobs, so therefore synchronize.
      synchronized {
        // These also have to be locked because a different process is checking on them.
        LockUtils.withLock(readingFile, Extensions.lock) {
          FileUtils.printWriterFromFile(readingFile).autoClose { printWriter =>
            new JLDCorpus(annotatedDocument).serialize(printWriter)
          }
        }
      }

      ontologyIds.foreach { ontologyId =>
        val outputFile = fileName.addExt(Extensions.jsonld).setName(1, ontologyId).distinguish(EidosLoopApp2.outputStage, outputDistinguisher).setDir(outputDir).toFile

        ground(ontologyId, annotatedDocument)

        LockUtils.withLock(outputFile, Extensions.lock) {
          FileUtils.printWriterFromFile(outputFile).autoClose { printWriter =>
            new JLDCorpus(annotatedDocument).serialize(printWriter)
          }
        }
      }

      val doneFile = fileName.distinguish(EidosLoopApp2.outputStage, doneDistinguisher).setDir(doneDir).toFile
      FileUtils.rename(file, doneFile)
    }
    catch {
      case exception: Exception =>
        EidosLoopApp2.logger.error(s"Exception for file $file", exception)
    }
  }

  val thread: SafeThread = new SafeThread(EidosLoopApp2.logger, interactive, waitDuration) {
    val filesBeingProcessed: MutableHashSet[String] = new MutableHashSet[String]
    val threadPoolExecutor = Executors.newFixedThreadPool(threads)

    override def shutdown(): Unit = {
      threadPoolExecutor.shutdown()
    }

    override def runSafely(): Unit = {
      val inputExtensions = Seq(Extensions.rd, Extensions.gnd)
      val outputExtensions = inputExtensions.map(_ + "." + Extensions.jsonld)
      val outputDistinguisher = FileName.getDistinguisher(EidosLoopApp2.outputStage, FileUtils.findFiles(outputDir, outputExtensions))
      val doneDistinguisher = FileName.getDistinguisher(EidosLoopApp2.outputStage, FileUtils.findFiles(doneDir, inputExtensions))

      while (!isInterrupted) {
        val files = EidosLoopApp2.synchronized {
          val allFiles = LockUtils.findFiles(inputDir, inputExtensions, Extensions.lock)
          val newFiles = allFiles.filter { file => !filesBeingProcessed.contains(file.getAbsolutePath) }
          val newAbsolutePaths = newFiles.map(_.getAbsolutePath)

          filesBeingProcessed ++= newAbsolutePaths
          newFiles
        }

        files.foreach { file =>
          threadPoolExecutor.execute(
            new Runnable() {
              def run: Unit = {
                if (file.getName.endsWith(Extensions.rd))
                  processReadingFile(file, filesBeingProcessed, outputDistinguisher, doneDistinguisher)
                else if (file.getName.endsWith(Extensions.gnd))
                  processGroundingFile(file, filesBeingProcessed, outputDistinguisher, doneDistinguisher)
                // TODO: What happens on an error in the above?
                synchronized { filesBeingProcessed -= file.getAbsolutePath }
              }
            }
          )
        }
        Thread.sleep(pauseDuration)
      }
    }
  }
}

object EidosLoopApp2 extends LoopApp {
  var useReal = DevtimeConfig.useReal

  // These will be used for the distinguishers and are their indexes.
  val inputStage = Stages.eidosInputStage
  val outputStage = Stages.eidosOutputStage

  def main(args: Array[String]): Unit = {
    AppEnvironment.setEnv {
      val baseDir = "./corpora/feb2022exp1"
      Map(
        "REST_CONSUMER_DOCUMENT_SERVICE" -> "",
        "REST_CONSUMER_ONTOLOGY_SERVICE" -> "",
        "REST_CONSUMER_USERNAME" -> "eidos",
        "REST_CONSUMER_PASSWORD" -> System.getenv("PASSWORD"),

        "EIDOS_INPUT_DIR" -> s"$baseDir/restconsumer/output",
        "EIDOS_OUTPUT_DIR" -> s"$baseDir/eidos/output",
        "EIDOS_DONE_DIR" -> s"$baseDir/restconsumer/done",

        "DOCUMENT_DIR" -> s"$baseDir/documents",
        "ONTOLOGY_DIR" -> s"$baseDir/ontologies",
        "READING_DIR" -> s"$baseDir/readings",
        "MOCK_DIR" -> s"$baseDir",

        "EIDOS_THREADS" -> "4"
      )
    }

    val  inputDir: String = getArgOrEnv(args, 0, "EIDOS_INPUT_DIR")
    val outputDir: String = getArgOrEnv(args, 1, "EIDOS_OUTPUT_DIR")
    val   doneDir: String = getArgOrEnv(args, 2, "EIDOS_DONE_DIR")

    val documentDir: String = getArgOrEnv(args, 3, "DOCUMENT_DIR")
    val ontologyDir: String = getArgOrEnv(args, 4, "ONTOLOGY_DIR")
    val  readingDir: String = getArgOrEnv(args, 5, "READING_DIR")

    val   threads: Int    = getArgOrEnv(args, 6, "EIDOS_THREADS").toInt

    FileUtils.ensureDirsExist(inputDir, outputDir, doneDir)
    loop {
      () => new EidosLoopApp2(inputDir, outputDir, doneDir,
          documentDir, ontologyDir, readingDir, threads).thread
    }
  }
}
