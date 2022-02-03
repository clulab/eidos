package org.clulab.wm.wmexchanger2.wmproducer

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.clulab.wm.eidoscommon.utils.Counter
import org.clulab.wm.eidoscommon.utils.{FileUtils, LockUtils}
import org.clulab.wm.wmexchanger.utils.DevtimeConfig
import org.clulab.wm.wmexchanger.utils.Extensions
import org.clulab.wm.wmexchanger.utils.LoopApp
import org.clulab.wm.wmexchanger.utils.SafeThread
import org.clulab.wm.wmexchanger2.utils.AppEnvironment
import org.clulab.wm.wmexchanger2.utils.FileName
import org.clulab.wm.wmexchanger2.utils.Stages

import java.io.File
import scala.util.Try

class RestProducerLoopApp(inputDir: String, doneDir: String) {
  var useReal: Boolean = RestProducerLoopApp.useReal

  val config: Config = ConfigFactory.defaultApplication().resolve()
  val service: String = config.getString("rest.producer.service")
  val interactive: Boolean = config.getBoolean("rest.producer.interactive")
  val waitDuration: Int = config.getInt("rest.producer.duration.wait")
  val pauseDuration: Int = config.getInt("rest.producer.duration.pause")
  val username: String = Try(config.getString("rest.producer.username")).getOrElse("")
  val password: String = Try(config.getString("rest.producer.password")).getOrElse("")
  val eidosVersion: String = config.getString("rest.producer.eidosVersion")
  val ontologyVersion: String = config.getString("rest.producer.ontologyVersion")

  val thread: SafeThread = new SafeThread(RestProducerLoopApp.logger, interactive, waitDuration) {

    def processFile(file: File, restProducer: RestProducerish, doneDistinguisher: Counter): Unit = {
      try {
        RestProducerLoopApp.logger.info(s"Uploading ${file.getName}")
        val fileName = FileName(file)
        val documentId = fileName.getDocumentId
        val ontologyId = fileName.getOntologyId
        val storageKey = restProducer.upload(file, documentId, ontologyId)

        RestProducerLoopApp.logger.info(s"Reporting storage key $storageKey for ${file.getName}")

        val doneFile = fileName.distinguish(RestProducerLoopApp.outputStage, doneDistinguisher).setDir(doneDir).toFile
        FileUtils.rename(file, doneFile)
      }
      catch {
        case exception: Exception =>
          RestProducerLoopApp.logger.error(s"Exception for file $file", exception)
      }
    }

    override def runSafely(): Unit = {
      val restProducer =
          if (useReal) new RealRestProducer(service, username, password, eidosVersion, ontologyVersion)
          else new MockRestProducer()
      val doneDistinguisher = FileName.getDistinguisher(RestProducerLoopApp.outputStage, FileUtils.findFiles(doneDir,
          Extensions.jsonld))
      val printWriter = FileUtils.appendingPrintWriterFromFile(doneDir + "/log.txt")

      def close(): Unit = {
        restProducer.close()
        printWriter.close()
      }

      // autoClose isn't executed if the thread is shot down, so this hook is included just in case.
      sys.ShutdownHookThread { close() }

      while (!isInterrupted) {
        val files = LockUtils.findFiles(inputDir, Extensions.jsonld, Extensions.lock)

        if (files.nonEmpty) {
          restProducer.open()

          files.par.foreach { file =>
            val storageKey = processFile(file, restProducer, doneDistinguisher)

            synchronized {
              printWriter.println(s"${file.getAbsolutePath}\t$storageKey")
            }
          }
        }
        else
          close()
        Thread.sleep(pauseDuration)
      }
      close()
    }
  }
}

object RestProducerLoopApp extends LoopApp {
  var useReal: Boolean = DevtimeConfig.useReal

  // These will be used for the distinguishers and are their indexes.
  val inputStage = Stages.restProducerInputStage
  val outputStage = Stages.restProducerOutputStage

  def main(args: Array[String]): Unit = {
    AppEnvironment.setEnv {
      Map.empty
    }

    val inputDir = getArgOrEnv(args, 0, "REST_PRODUCER_INPUT_DIR")
    val  doneDir = getArgOrEnv(args, 1, "REST_PRODUCER_DONE_DIR")

    FileUtils.ensureDirsExist(inputDir, doneDir)
    loop {
      () => new RestProducerLoopApp(inputDir, doneDir).thread
    }
  }
}
