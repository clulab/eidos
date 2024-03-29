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
import org.clulab.wm.eidoscommon.utils.StringUtils
import org.clulab.wm.eidoscommon.utils.{FileEditor, FileUtils, LockUtils}
import org.clulab.wm.wmexchanger.utils.DevtimeConfig
import org.clulab.wm.wmexchanger.utils.Extensions
import org.clulab.wm.wmexchanger.utils.LoopApp
import org.clulab.wm.wmexchanger.utils.SafeThread
import org.clulab.wm.wmexchanger2.utils.AppEnvironment
import org.clulab.wm.wmexchanger2.utils.FileName
import org.clulab.wm.wmexchanger2.utils.OntologyMap
import org.clulab.wm.wmexchanger2.utils.Stages

import java.io.File
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import scala.collection.mutable
import scala.collection.mutable.{HashSet => MutableHashSet}

class EidosLoopApp2(inputDir: String, outputDir: String, doneDir: String,
                    documentDir: String, ontologyDir: String, readingDir: String, threads: Int) {
  var useReal: Boolean = EidosLoopApp2.useReal

  val config: Config = ConfigFactory.defaultApplication().resolve().getConfig("eidos")
  val interactive: Boolean = config.getBoolean("interactive")
  val waitDuration: Int = config.getInt("duration.wait")
  val pauseDuration: Int = config.getInt("duration.pause")

  val options: EidosOptions = EidosOptions()
  val reader: EidosSystemish =
      if (useReal) new RealEidosSystem()
      else new MockEidosSystem(System.getenv("MOCK_DIR"))
  val deserializer = new JLDDeserializer()
  val ontologyMap = new OntologyMap(reader, ontologyDir, EidosLoopApp2.logger)

  def ground(ontologyId: String, annotatedDocument: AnnotatedDocument): AnnotatedDocument = {
    val ontologyHandler = ontologyMap(ontologyId)

    annotatedDocument.allEidosMentions.foreach(ontologyHandler.ground)
    annotatedDocument
  }

  def processGroundingFile(file: File, outputDistinguisher: Counter, doneDistinguisher: Counter): Unit = {
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
        val outputFile = fileName.setExt(Extensions.jsonld).setName(1, ontologyId).distinguish(EidosLoopApp2.outputStage, outputDistinguisher).setDir(outputDir).toFile

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

  def processReadingFile(file: File, outputDistinguisher: Counter, doneDistinguisher: Counter): Unit = {
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

      // These also have to be locked because a different process is checking on them.
      LockUtils.withLock(readingFile, Extensions.lock) {
        FileUtils.printWriterFromFile(readingFile).autoClose { printWriter =>
          new JLDCorpus(annotatedDocument).serialize(printWriter)
        }
      }

      ontologyIds.foreach { ontologyId =>
        val outputFile = fileName.setExt(Extensions.jsonld).setName(1, ontologyId).distinguish(EidosLoopApp2.outputStage, outputDistinguisher).setDir(outputDir).toFile

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
    val threadPoolExecutor: ExecutorService = Executors.newFixedThreadPool(threads)
    val documentsBeingRead: mutable.Set[String] = mutable.Set.empty

    override def shutdown(): Unit = {
      threadPoolExecutor.shutdown()
    }

    override def runSafely(): Unit = {
      val outputDistinguisher = FileName.getDistinguisher(EidosLoopApp2.outputStage, FileUtils.findFiles(outputDir, Extensions.jsonld))
      val doneDistinguisher = FileName.getDistinguisher(EidosLoopApp2.outputStage, FileUtils.findFiles(doneDir, Extensions.txt))
      // This doesn't work when placed above.
      val documentsAlreadyRead: mutable.Set[String] = FileUtils.findFiles(readingDir, Extensions.jsonld).map { file =>
        StringUtils.beforeFirst(file.getName, '.')
      }.to[mutable.Set]

      while (!isInterrupted) {
        val files = EidosLoopApp2.synchronized {
          val allFiles = LockUtils.findFiles(inputDir, Extensions.txt, Extensions.lock).take(EidosLoopApp2.fileLimit)
          val newFiles = EidosLoopApp2.synchronized {
            allFiles.filter { file => !filesBeingProcessed.contains(file.getAbsolutePath) }
          }

          newFiles
        }

        files.foreach { file =>
          val name = file.getName
          val fileName = FileName(file)
          val documentId = fileName.getDocumentId
          val (isAlreadyRead, isBeingRead) = EidosLoopApp2.synchronized {
            (documentsAlreadyRead(documentId), documentsBeingRead(documentId))
          }

          if (isAlreadyRead) {
            EidosLoopApp2.logger.info(s"Queuing for grounding $name...")
            EidosLoopApp2.synchronized { filesBeingProcessed += file.getAbsolutePath }
            threadPoolExecutor.execute(
              new NamedRunnable(name, "grounding") {
                def run(): Unit = {
                  EidosLoopApp2.logger.info(s"Grounding $name...")
                  processGroundingFile(file, outputDistinguisher, doneDistinguisher)
                  EidosLoopApp2.synchronized { filesBeingProcessed -= file.getAbsolutePath }
                }
              }
            )
          }
          else if (!isBeingRead) {
            EidosLoopApp2.logger.info(s"Queuing for reading $name...")
            EidosLoopApp2.synchronized {
              filesBeingProcessed += file.getAbsolutePath
              documentsBeingRead += documentId
            }
            threadPoolExecutor.execute(
              new NamedRunnable(name, "reading") {

                def run(): Unit = {
                  EidosLoopApp2.logger.info(s"Reading $name...")
                  // Open and close so that -= is guaranteed
                  processReadingFile(file, outputDistinguisher, doneDistinguisher)
                  EidosLoopApp2.synchronized {
                    filesBeingProcessed -= file.getAbsolutePath
                    documentsBeingRead -= documentId
                    documentsAlreadyRead += documentId
                  }
                }
              }
            )
          }
          else
            EidosLoopApp2.logger.info(s"Not yet queuing $name...")
        }
        EidosLoopApp2.synchronized {
          val documentsBeingGrounded = filesBeingProcessed.size - documentsBeingRead.size
          println("-------------------------------")
          println(s"files: ${files.length}")
          println(s"filesBeingProcessed: ${filesBeingProcessed.size}")
          println(s"documentsBeingRead: ${documentsBeingRead.size}")
          println(s"documentsBeingGrounded: $documentsBeingGrounded")
          println(s"documentsAlreadyRead: ${documentsAlreadyRead.size}")
          println("-------------------------------")
        }
        Thread.sleep(pauseDuration)
      }
    }
  }
}

object EidosLoopApp2 extends LoopApp {
  var useReal: Boolean = DevtimeConfig.useReal
  val fileLimit = 5000

  // These will be used for the distinguishers and are their indexes.
  val inputStage: Int = Stages.eidosInputStage
  val outputStage: Int = Stages.eidosOutputStage

  def main(args: Array[String]): Unit = {

    if (false) {
      AppEnvironment.setEnv {
        val baseDir = "../corpora/feb2022exp1"
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
    }

    val  inputDir: String = getArgOrEnv(args, 0, "EIDOS_INPUT_DIR")
    val outputDir: String = getArgOrEnv(args, 1, "EIDOS_OUTPUT_DIR")
    val   doneDir: String = getArgOrEnv(args, 2, "EIDOS_DONE_DIR")

    val documentDir: String = getArgOrEnv(args, 3, "DOCUMENT_DIR")
    val ontologyDir: String = getArgOrEnv(args, 4, "ONTOLOGY_DIR")
    val  readingDir: String = getArgOrEnv(args, 5, "READING_DIR")

    val   threads: Int    = getArgOrEnv(args, 6, "EIDOS_THREADS").toInt

    FileUtils.ensureDirsExist(inputDir, outputDir, doneDir, documentDir, ontologyDir, readingDir)
    loop {
      () => new EidosLoopApp2(inputDir, outputDir, doneDir,
          documentDir, ontologyDir, readingDir, threads).thread
    }
  }
}

abstract class NamedRunnable(val name: String, val purpose: String) extends Runnable
