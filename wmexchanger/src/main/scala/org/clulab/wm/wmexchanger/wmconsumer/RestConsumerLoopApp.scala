package org.clulab.wm.wmexchanger.wmconsumer

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.PropertiesBuilder
import org.clulab.wm.wmexchanger.utils.SafeThread
import org.clulab.wm.eidoscommon.utils.Sinker
import org.clulab.wm.wmexchanger.utils.DevtimeConfig
import org.clulab.wm.wmexchanger.utils.Extensions
import org.clulab.wm.wmexchanger.utils.LockUtils
import org.clulab.wm.wmexchanger.utils.LoopApp

import java.io.File
import java.util.Properties

// See https://hc.apache.org/httpcomponents-client-ga/tutorial/html/authentication.html
// and https://mkyong.com/java/apache-httpclient-basic-authentication-examples/
class RestConsumerLoopApp(inputDir: String, outputDir: String, doneDir: String) {
  var useReal = RestConsumerLoopApp.useReal

  val config: Config = ConfigFactory.defaultApplication().resolve()
  val service: String = config.getString("rest.consumer.service")
  val annotations: Boolean = config.getBoolean("rest.consumer.annotations")
  val interactive: Boolean = config.getBoolean("rest.consumer.interactive")
  val waitDuration: Int = config.getInt("rest.consumer.duration.wait")
  val pauseDuration: Int = config.getInt("rest.consumer.duration.pause")
  val username: String = config.getString("rest.consumer.username")
  val password: String = config.getString("rest.consumer.password")

  val thread: SafeThread = new SafeThread(RestConsumerLoopApp.logger, interactive, waitDuration) {

    def processFile(restConsumer: RestConsumerish, file: File): Unit = {
      try {
        RestConsumerLoopApp.logger.info(s"Downloading ${file.getName}")
        val cdr = restConsumer.download(file)
        val outputFile = FileEditor(file).setDir(outputDir).get

        Sinker.printWriterFromFile(outputFile, append = false).autoClose { printWriter =>
          printWriter.print(cdr)
        }

        val lockFile = FileEditor(outputFile).setExt(Extensions.lock).get
        lockFile.createNewFile()

        val doneFile = FileEditor(file).setDir(doneDir).get
        FileUtils.rename(file, doneFile)
      }
      catch {
        case exception: Exception =>
          RestConsumerLoopApp.logger.error(s"Exception for file $file", exception)
      }
    }

    override def runSafely(): Unit = {
      val restConsumer =
          if (useReal) new RealRestConsumer(service, username, password, annotations)
          else new MockRestConsumer(outputDir)

      // autoClose isn't executed if the thread is shot down, so this hook is included just in case.
      sys.ShutdownHookThread {
        restConsumer.close()
      }

      while (!isInterrupted) {
        LockUtils.cleanupLocks(outputDir, Extensions.lock, Extensions.json)

        val files = LockUtils.findFiles(inputDir, Extensions.json, Extensions.lock).par

        if (files.nonEmpty) {
          restConsumer.open()

          files.foreach { file =>
            processFile(restConsumer, file)
          }
        }
        else
          restConsumer.close()
        Thread.sleep(pauseDuration)
      }
      restConsumer.close()
    }
  }
}

object RestConsumerLoopApp extends LoopApp {
  var useReal = DevtimeConfig.useReal

  def main(args: Array[String]): Unit = {
    val  inputDir: String = getArgOrEnv(args, 0, "REST_CONSUMER_INPUT_DIR")
    val outputDir: String = getArgOrEnv(args, 1, "REST_CONSUMER_OUTPUT_DIR")
    val   doneDir: String = getArgOrEnv(args, 2, "REST_CONSUMER_DONE_DIR")

    FileUtils.ensureDirsExist(inputDir, outputDir, doneDir)
    loop {
      () => new RestConsumerLoopApp(inputDir, outputDir, doneDir).thread
    }
  }
}
