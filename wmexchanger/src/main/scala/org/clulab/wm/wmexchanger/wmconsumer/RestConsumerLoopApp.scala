package org.clulab.wm.wmexchanger.wmconsumer

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileEditor
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
  val useReal = DevtimeConfig.useReal

  val config: Config = ConfigFactory.load("restconsumer")
  val service: String = config.getString("RestConsumerApp.service")
  val annotations: Boolean = config.getBoolean("RestConsumerApp.annotations")
  val login: String = config.getString("RestConsumerApp.login")
  val interactive: Boolean = config.getBoolean("RestConsumerApp.interactive")
  val waitDuration: Int = config.getInt("RestConsumerApp.duration.wait")
  val pauseDuration: Int = config.getInt("RestConsumerApp.duration.pause")
  val properties: Properties = PropertiesBuilder.fromFile(login).get
  val username: String = properties.getProperty("username")
  val password: String = properties.getProperty("password")

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
        if (doneFile.exists) doneFile.delete
        file.renameTo(doneFile)
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

object RestConsumerLoopApp extends App with LoopApp {
  val inputDir: String = args(0)
  val outputDir: String = args(1)
  val doneDir: String = args(2)

  loop {
    () => new RestConsumerLoopApp(inputDir, outputDir, doneDir).thread
  }
}
