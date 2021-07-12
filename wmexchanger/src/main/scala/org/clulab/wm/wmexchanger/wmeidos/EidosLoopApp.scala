package org.clulab.wm.wmexchanger.wmeidos

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.clulab.wm.eidos.EidosOptions
import org.clulab.wm.eidos.serialization.jsonld.JLDCorpus
import org.clulab.wm.eidos.utils.meta.CdrText
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.StringUtils
import org.clulab.wm.wmexchanger.utils.DevtimeConfig
import org.clulab.wm.wmexchanger.utils.Extensions
import org.clulab.wm.wmexchanger.utils.LockUtils
import org.clulab.wm.wmexchanger.utils.LoopApp
import org.clulab.wm.wmexchanger.utils.SafeThread

import java.io.File
import java.util.concurrent.Executors
import scala.collection.mutable.{HashSet => MutableHashSet}

class EidosLoopApp(inputDir: String, outputDir: String, doneDir: String, threads: Int) {
  var useReal = EidosLoopApp.useReal

  val config: Config = ConfigFactory.defaultApplication().resolve().getConfig("eidos")
  val interactive: Boolean = config.getBoolean("interactive")
  val waitDuration: Int = config.getInt("duration.wait")
  val pauseDuration: Int = config.getInt("duration.pause")

  val options: EidosOptions = EidosOptions()
  val reader =
      if (useReal) new RealEidosSystem()
      else new MockEidosSystem()

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

      LockUtils.withLock(outputFile, Extensions.lock) {
        FileUtils.printWriterFromFile(outputFile).autoClose { printWriter =>
          new JLDCorpus(annotatedDocument).serialize(printWriter)
        }
      }

      EidosLoopApp.synchronized {
        val doneFile = FileEditor(file).setDir(doneDir).get
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
              def run: Unit = processFile(file, filesBeingProcessed)
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
    val   threads: Int    = getArgOrEnv(args, 3, "EIDOS_THREADS").toInt

    FileUtils.ensureDirsExist(inputDir, outputDir, doneDir)
    loop {
      () => new EidosLoopApp(inputDir, outputDir, doneDir, threads).thread
    }
  }
}
