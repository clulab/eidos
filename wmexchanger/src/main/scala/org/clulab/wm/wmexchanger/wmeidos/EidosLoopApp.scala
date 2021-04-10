package org.clulab.wm.wmexchanger.wmeidos

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.clulab.processors.Document
import org.clulab.wm.eidos.EidosOptions
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.Metadata
import org.clulab.wm.eidos.serialization.jsonld.JLDCorpus
import org.clulab.wm.eidos.utils.meta.CdrText
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.StringUtils
import org.clulab.wm.wmexchanger.utils.Extensions
import org.clulab.wm.wmexchanger.utils.LockUtils
import org.clulab.wm.wmexchanger.utils.LoopApp
import org.clulab.wm.wmexchanger.utils.SafeThread
import org.clulab.wm.wmexchanger.wmconsumer.RestConsumerLoopApp

import java.io.File
import java.util.concurrent.Executors
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.SynchronousQueue
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit
import scala.collection.mutable.{HashSet => MutableHashSet}

class Reader {
//  val eidosSystem = new EidosSystem()

  def getEmptyAnnotatedDocument(idOpt: Option[String]): AnnotatedDocument = {
    val document = new Document(Array.empty)
    document.id = idOpt

    val annotatedDocument = AnnotatedDocument(document, Seq.empty)

    annotatedDocument
  }

  def extractFromText(text: String, options: EidosOptions, metadata: Metadata): AnnotatedDocument = {
    // eidosSystem.extractFromText(text, options, metadata)
    Thread.sleep(5000)
    getEmptyAnnotatedDocument(metadata.idOpt)
  }
}

class EidosLoopApp(inputDir: String, outputDir: String, doneDir: String, threads: Int) {
  val config: Config = ConfigFactory.load("eidos")
  val interactive: Boolean = config.getBoolean("Eidos.interactive")
  val waitDuration: Int = config.getInt("Eidos.duration.wait")
  val pauseDuration: Int = config.getInt("Eidos.duration.pause")

  val options: EidosOptions = EidosOptions()
  val reader = new Reader()

  def processFile(file: File, filesBeingProcessed: MutableHashSet[String]): Unit = {
    try {
      val annotatedDocument =
        try {
          val eidosText = CdrText(file)
          val text = eidosText.getText
          val metadata = eidosText.getMetadata

          reader.extractFromText(text, options, metadata)
        }
        catch {
          case exception: Throwable =>
            EidosLoopApp.logger.error(s"Exception for file $file", exception)
            reader.getEmptyAnnotatedDocument(Some(StringUtils.afterFirst(file.getName, '.', true)))
        }
      val outputFile = FileEditor(file).setDir(outputDir).setExt(Extensions.jsonld).get

      FileUtils.printWriterFromFile(outputFile).autoClose { printWriter =>
        new JLDCorpus(annotatedDocument).serialize(printWriter)
      }

      val lockFile = FileEditor(outputFile).setExt(Extensions.lock).get
      lockFile.createNewFile()

      EidosLoopApp.synchronized {
        val doneFile = FileEditor(file).setDir(doneDir).get
        if (doneFile.exists) doneFile.delete
        file.renameTo(doneFile)
        filesBeingProcessed -= file.getAbsolutePath
      }
    }
    catch {
      case exception: Exception =>
        EidosLoopApp.logger.error(s"Exception for file $file", exception)
    }
  }

  val thread: SafeThread = new SafeThread(RestConsumerLoopApp.logger, interactive, waitDuration) {
    val filesBeingProcessed: MutableHashSet[String] = new MutableHashSet[String]
    val threadPoolExecutor = Executors.newFixedThreadPool(2) // threads)

    override def shutdown(): Unit = {
      threadPoolExecutor.shutdown()
    }

    override def runSafely(): Unit = {
      while (!isInterrupted) {
        LockUtils.cleanupLocks(outputDir, Extensions.lock, Extensions.jsonld)

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
              def run: Unit = processFile(file, filesBeingProcessed)
            }
          )
        }
        Thread.sleep(pauseDuration)
      }
    }
  }
}

object EidosLoopApp extends App with LoopApp {
  val inputDir: String = args(0)
  val outputDir: String = args(1)
  val doneDir: String = args(2)
  val threads: Int = args(3).toInt

  loop {
    () => new EidosLoopApp(inputDir, outputDir, doneDir, threads).thread
  }
}
