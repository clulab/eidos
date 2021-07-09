package org.clulab.wm.wmexchanger.wmproducer

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.PropertiesBuilder
import org.clulab.wm.wmexchanger.utils.DevtimeConfig
import org.clulab.wm.wmexchanger.utils.Extensions
import org.clulab.wm.wmexchanger.utils.LockUtils
import org.clulab.wm.wmexchanger.utils.LoopApp
import org.clulab.wm.wmexchanger.utils.SafeThread

import java.io.File
import java.util.Properties

class RestProducerLoopApp(inputDir: String, doneDir: String) {
  var useReal: Boolean = RestProducerLoopApp.useReal

  val config: Config = ConfigFactory.defaultApplication().resolve()
  val service: String = config.getString("rest.producer.service")
  val interactive: Boolean = config.getBoolean("rest.producer.interactive")
  val waitDuration: Int = config.getInt("rest.producer.duration.wait")
  val pauseDuration: Int = config.getInt("rest.producer.duration.pause")
  val username: String = config.getString("rest.producer.username")
  val password: String = config.getString("rest.producer.password")
  val eidosVersion: String = config.getString("rest.producer.eidosVersion")
  val ontologyVersion: String = config.getString("rest.producer.ontologyVersion")

  val thread: SafeThread = new SafeThread(RestProducerLoopApp.logger, interactive, waitDuration) {

    def processFile(restProducer: RestProducerish, file: File): Unit = {
      try {
        RestProducerLoopApp.logger.info(s"Uploading ${file.getName}")
        val storageKey = restProducer.upload(file)

        RestProducerLoopApp.logger.info(s"Reporting storage key $storageKey for ${file.getName}")

        val doneFile = FileEditor(file).setDir(doneDir).get
        FileUtils.rename(file, doneFile)
      }
      catch {
        case exception: Exception =>
          RestProducerLoopApp.logger.error(s"Exception for file $file", exception)
      }
    }

    override def runSafely(): Unit = {
      val restProducer =
          if (useReal) new RealRestProducer(service, username, password, eidosVersion)
          else new MockRestProducer()

      // autoClose isn't executed if the thread is shot down, so this hook is included just in case.
      sys.ShutdownHookThread {
        restProducer.close()
      }

      while (!isInterrupted) {
        val files = LockUtils.findFiles(inputDir, Extensions.jsonld, Extensions.lock)

        if (files.nonEmpty) {
          restProducer.open()

          files.par.foreach { file =>
            processFile(restProducer, file)
          }
        }
        else
          restProducer.close()
        Thread.sleep(pauseDuration)
      }
      restProducer.close()
    }
  }
}

object RestProducerLoopApp extends LoopApp {
  var useReal: Boolean = DevtimeConfig.useReal

  def main(args: Array[String]): Unit = {
    val inputDir = getArgOrEnv(args, 0, "REST_PRODUCER_INPUT_DIR")
    val  doneDir = getArgOrEnv(args, 1, "REST_PRODUCER_DONE_DIR")

    FileUtils.ensureDirsExist(inputDir, doneDir)
    loop {
      () => new RestProducerLoopApp(inputDir, doneDir).thread
    }
  }
}
