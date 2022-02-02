package org.clulab.wm.wmexchanger2.wmeidos

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.clulab.wm.eidos.EidosOptions
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

import java.io.File
import java.nio.file.Files
import java.util.concurrent.Executors
import scala.collection.mutable.{HashSet => MutableHashSet}

class EidosLoopApp(inputDir: String, outputDir: String, doneDir: String,
    documentDir: String, ontologyDir: String, readingDir: String, threads: Int) {
  var useReal = EidosLoopApp.useReal

  val config: Config = ConfigFactory.defaultApplication().resolve().getConfig("eidos")
  val interactive: Boolean = config.getBoolean("interactive")
  val waitDuration: Int = config.getInt("duration.wait")
  val pauseDuration: Int = config.getInt("duration.pause")

  val options: EidosOptions = EidosOptions()
  val reader =
      if (useReal) new RealEidosSystem()
      else new MockEidosSystem()
  val deserializer = new JLDDeserializer()

  // TODO: If number of ontology files is small, could have map from id to processed thing

  def processGroundingFile(file: File, filesBeingProcessed: MutableHashSet[String], outputDistinguisher: Counter, doneDistinguisher: Counter): Unit = {
    try {
      val documentId = StringUtils.beforeFirst(file.getName, '.')
      val ontologyIds = Sourcer.sourceFromFile(file).autoClose { source =>
        source.getLines.toVector
      }
      val documentFile = new File(readingDir + "/" + documentId + Extensions.jsonld)
      val ontologyFiles = ontologyIds.map { ontologyId =>
        new File(ontologyDir + "/" + ontologyId + Extensions.yml)
      }

      val annotatedDocument =
        try {
          val json = FileUtils.getTextFromFile(documentFile)

          deserializer.deserialize(json).head
        }
        catch {
          case exception: Throwable =>
            EidosLoopApp.logger.error(s"Exception for file $file", exception)
            reader.getEmptyAnnotatedDocument(Some(StringUtils.afterFirst(file.getName, '.', true)))
        }

      ontologyFiles.foreach { ontologyFile =>
        val ontologyId = StringUtils.beforeFirst(ontologyFile.getName, '.')
        val groundedOutputFile = FileEditor(file).setName(documentId + "_" + ontologyId).setExt(Extensions.gnd_jsonld).distinguish(outputDistinguisher.getAndInc).setDir(outputDir).get

        Files.copy(documentFile.toPath, groundedOutputFile.toPath)
      }
      
      EidosLoopApp.synchronized {
        val doneFile = FileEditor(file).distinguish(doneDistinguisher.getAndInc).setDir(doneDir).get
        FileUtils.rename(file, doneFile)
        filesBeingProcessed -= file.getAbsolutePath
      }
    }
    catch {
      case exception: Exception =>
        EidosLoopApp.logger.error(s"Exception for file $file", exception)
    }
  }

  def processReadingFile(file: File, filesBeingProcessed: MutableHashSet[String], outputDistinguisher: Counter, doneDistinguisher: Counter): Unit = {
    try {
      val documentId = StringUtils.beforeFirst(file.getName, '.')
      val ontologyIds = Sourcer.sourceFromFile(file).autoClose { source =>
        source.getLines.toVector
      }
      val documentFile = new File(documentDir + "/" + documentId + Extensions.json)
      val ontologyFiles = ontologyIds.map { ontologyId =>
        new File(ontologyDir + "/" + ontologyId + Extensions.yml)
      }

      val annotatedDocument =
        try {
          val eidosText = CdrText(documentFile)
          val text = eidosText.getText
          val metadata = eidosText.getMetadata

          // Do first with no grounders,
          // Then go through grounders one at a time
          reader.extractFromText(text, options, metadata)
        }
        catch {
          case exception: Throwable =>
            EidosLoopApp.logger.error(s"Exception for file $file", exception)
            reader.getEmptyAnnotatedDocument(Some(StringUtils.afterFirst(file.getName, '.', true)))
        }
      val outputFile = FileEditor(documentFile).setExt(Extensions.jsonld).setDir(readingDir).get

      FileUtils.printWriterFromFile(outputFile).autoClose { printWriter =>
        new JLDCorpus(annotatedDocument).serialize(printWriter)
      }

      ontologyFiles.foreach { ontologyFile =>
          val ontologyId = StringUtils.beforeFirst(ontologyFile.getName, '.')
          // TODO Want to keep track of all distinguishing thing.
          val groundedOutputFile = FileEditor(file).setName(documentId + "_" + ontologyId).setExt(Extensions.rd_jsonld).distinguish(outputDistinguisher.getAndInc).setDir(outputDir).get

          Files.copy(outputFile.toPath, groundedOutputFile.toPath)
      }

      EidosLoopApp.synchronized {
        val doneFile = FileEditor(file).distinguish(doneDistinguisher.getAndInc).setDir(doneDir).get
        FileUtils.rename(file, doneFile)
        filesBeingProcessed -= file.getAbsolutePath
      }
    }
    catch {
      case exception: Exception =>
        EidosLoopApp.logger.error(s"Exception for file $file", exception)
    }
  }

  val thread: SafeThread = new SafeThread(EidosLoopApp.logger, interactive, waitDuration) {
    val filesBeingProcessed: MutableHashSet[String] = new MutableHashSet[String]
    val threadPoolExecutor = Executors.newFixedThreadPool(threads)

    override def shutdown(): Unit = {
      threadPoolExecutor.shutdown()
    }

    override def runSafely(): Unit = {
      val outputDistinguisher = Counter(FileUtils.distinguish(3, FileUtils.findFiles(outputDir,
          Seq(Extensions.rd_jsonld, Extensions.gnd_jsonld))))
      val doneDistinguisher = Counter(FileUtils.distinguish(3, FileUtils.findFiles(doneDir,
          Seq(Extensions.rd, Extensions.gnd))))

      while (!isInterrupted) {
        val files = EidosLoopApp.synchronized {
          val allFiles = LockUtils.findFiles(inputDir, Extensions.json, Extensions.lock)
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
                else
                  processGroundingFile(file, filesBeingProcessed, outputDistinguisher, doneDistinguisher)
              }
            }
          )
        }
        Thread.sleep(pauseDuration)
      }
    }
  }
}

object EidosLoopApp extends LoopApp {
  var useReal = DevtimeConfig.useReal

  def main(args: Array[String]): Unit = {
    val  inputDir: String = getArgOrEnv(args, 0, "EIDOS_INPUT_DIR")
    val outputDir: String = getArgOrEnv(args, 1, "EIDOS_OUTPUT_DIR")
    val   doneDir: String = getArgOrEnv(args, 2, "EIDOS_DONE_DIR")

    val documentDir: String = getArgOrEnv(args, 3, "DOCUMENT_DIR")
    val ontologyDir: String = getArgOrEnv(args, 4, "ONTOLOGY_DIR")
    val  readingDir: String = getArgOrEnv(args, 5, "READING_DIR")

    val   threads: Int    = getArgOrEnv(args, 3, "EIDOS_THREADS").toInt

    FileUtils.ensureDirsExist(inputDir, outputDir, doneDir)
    loop {
      () => new EidosLoopApp(inputDir, outputDir, doneDir,
          documentDir, ontologyDir, readingDir, threads).thread
    }
  }
}
