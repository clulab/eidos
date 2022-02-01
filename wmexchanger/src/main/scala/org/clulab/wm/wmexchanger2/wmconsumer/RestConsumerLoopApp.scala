package org.clulab.wm.wmexchanger2.wmconsumer

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.Counter
import org.clulab.wm.eidoscommon.utils.Holder
import org.clulab.wm.eidoscommon.utils.StringUtils
import org.clulab.wm.eidoscommon.utils.{FileEditor, FileUtils, LockUtils, Sinker}
import org.clulab.wm.wmexchanger.utils.SafeThread
import org.clulab.wm.wmexchanger.utils.DevtimeConfig
import org.clulab.wm.wmexchanger.utils.Extensions
import org.clulab.wm.wmexchanger.utils.LoopApp
import org.json4s.DefaultFormats
import org.json4s.JArray
import org.json4s.JValue
import org.json4s.jackson.JsonMethods

import java.io.File
import scala.collection.mutable
import scala.util.Try

// See https://hc.apache.org/httpcomponents-client-ga/tutorial/html/authentication.html
// and https://mkyong.com/java/apache-httpclient-basic-authentication-examples/
class RestConsumerLoopApp(inputDir: String, outputDir: String, doneDir: String,
    documentDir: String, ontologyDir: String, readingDir: String) {
  var useReal: Boolean = RestConsumerLoopApp.useReal

  val config: Config = ConfigFactory.defaultApplication().resolve()
  val cdrService: String = config.getString("rest.consumer.cdrService")
  val ontologyService: String = config.getString("rest.consumer.ontologyService")
  val annotations: Boolean = config.getBoolean("rest.consumer.annotations")
  val interactive: Boolean = config.getBoolean("rest.consumer.interactive")
  val waitDuration: Int = config.getInt("rest.consumer.duration.wait")
  val pauseDuration: Int = config.getInt("rest.consumer.duration.pause")
  val username: String = Try(config.getString("rest.consumer.username")).getOrElse("")
  val password: String = Try(config.getString("rest.consumer.password")).getOrElse("")

  val thread: SafeThread = new SafeThread(RestConsumerLoopApp.logger, interactive, waitDuration) {

    def processRequest(restConsumerRequest: RestConsumerRequest, outputDistinguisher: Counter,
        doneDistinguisher: Counter): Unit = {
      val file = restConsumerRequest.file
      val readingFile = FileEditor(file).setDir(readingDir).setExt(Extensions.jsonld).get
      val outputFile =
          if (readingFile.exists)
            FileEditor(file).setExt(Extensions.gnd).distinguish(outputDistinguisher.get).setDir(outputDir).get
          else
            FileEditor(file).setExt(Extensions.rd).distinguish(outputDistinguisher.get).setDir(outputDir).get

      outputDistinguisher.inc()
      LockUtils.withLock(outputFile, Extensions.lock) {
        Sinker.printWriterFromFile(outputFile, append = false).autoClose { printWriter =>
          restConsumerRequest.ontologyIds.foreach(printWriter.println)
        }
      }

      val doneFile = FileEditor(file).distinguish(doneDistinguisher.get).setDir(doneDir).get
      doneDistinguisher.inc()
      FileUtils.rename(file, doneFile)
    }

    def processFiles(files: Seq[File], knownDocumentIds: mutable.Set[String], knownOntologyIds: mutable.Set[String],
        restDocumentConsumer: RestConsumerish, restOntologyConsumer: RestConsumerish): Seq[RestConsumerRequest] = {
      implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

      val restConsumerRequests = files.map { file =>
        val documentId = StringUtils.beforeLast(file.getName, '.')
        val json = FileUtils.getTextFromFile(file)
        val jValue = JsonMethods.parse(json)
        val ontologyIds = {
          (jValue \ "value" \ "ontologies").extract[JArray].arr.map { ontology =>
            (ontology \ "ontology").extract[String]
          }
        }

        RestConsumerRequest(file, jValue, documentId, ontologyIds)
      }
      val documentIds = restConsumerRequests.map(_.documentId).toSet
      val ontologyIds = restConsumerRequests.flatMap(_.ontologyIds).toSet
      val unknownDocumentIds = documentIds -- knownDocumentIds
      val unknownOntologyIds = ontologyIds -- knownOntologyIds

      if (unknownDocumentIds.nonEmpty) {
        restDocumentConsumer.open()
        unknownDocumentIds.par.foreach { documentId =>
          RestConsumerLoopApp.logger.info(s"Downloading $documentId${Extensions.json}")
          val document = restDocumentConsumer.download(documentId)
          val outputFile = FileEditor(new File(documentId)).setDir(documentDir).setExt(Extensions.json).get

          Sinker.printWriterFromFile(outputFile, append = false).autoClose { printWriter =>
            printWriter.print(document)
          }
        }
        knownDocumentIds ++ unknownDocumentIds
      }

      if (unknownOntologyIds.nonEmpty) {
        restOntologyConsumer.open()
        unknownOntologyIds.par.foreach { ontologyId =>
          RestConsumerLoopApp.logger.info(s"Downloading $ontologyId${Extensions.yml}")
          val ontology = restOntologyConsumer.download(ontologyId)
          val outputFile = FileEditor(new File(ontologyId)).setDir(ontologyDir).setExt(Extensions.yml).get

          Sinker.printWriterFromFile(outputFile, append = false).autoClose { printWriter =>
            printWriter.print(ontology)
          }
        }
        knownOntologyIds ++ unknownOntologyIds
      }

      restConsumerRequests
    }

    override def runSafely(): Unit = {
      val restDocumentConsumer =
          if (useReal) new RealRestDocumentConsumer(cdrService, username, password, annotations)
          else new MockRestDocumentConsumer(documentDir)
      val restOntologyConsumer =
          if (useReal) new RealRestOntologyConsumer(ontologyService, username, password)
          else new MockRestOntologyConsumer(ontologyDir)
      val knownDocumentIds = LockUtils.findFiles(documentDir, Extensions.json, Extensions.lock)
          .map { documentFile => StringUtils.beforeLast(documentFile.getName, '.') }
          .to[mutable.Set]
      val knownOntologyIds = LockUtils.findFiles(ontologyDir, Extensions.yml, Extensions.lock)
          .map { ontologyFile => StringUtils.beforeLast(ontologyFile.getName, '.') }
          .to[mutable.Set]
      val outputDistinguisher = Counter(FileUtils.distinguish(2, FileUtils.findFiles(outputDir,
          Seq(Extensions.placeholder + Extensions.rd, Extensions.placeholder + Extensions.gnd))))
      val doneDistinguisher = Counter(FileUtils.distinguish(2, FileUtils.findFiles(doneDir,
          Extensions.placeholder + Extensions.json)))

      // autoClose isn't executed if the thread is shot down, so this hook is included just in case.
      sys.ShutdownHookThread {
        restDocumentConsumer.close()
        restOntologyConsumer.close()
      }

      while (!isInterrupted) {
        val files = LockUtils.findFiles(inputDir, Extensions.json, Extensions.lock).take(RestConsumerLoopApp.fileLimit)

        if (files.nonEmpty) {
          // These need holders.
          val restConsumerRequests = processFiles(files, knownDocumentIds, knownOntologyIds, restDocumentConsumer, restOntologyConsumer)

          restConsumerRequests.foreach { restConsumerRequest =>
            processRequest(restConsumerRequest, outputDistinguisher, doneDistinguisher)
          }
        }
        else {
          restDocumentConsumer.close()
          restOntologyConsumer.close()
        }
        Thread.sleep(pauseDuration)
      }
      restDocumentConsumer.close()
      restOntologyConsumer.close()
    }
  }
}

object RestConsumerLoopApp extends LoopApp {
  var useReal: Boolean = DevtimeConfig.useReal
  val fileLimit = 5000

  def main(args: Array[String]): Unit = {
    val  inputDir: String = getArgOrEnv(args, 0, "REST_CONSUMER_INPUT_DIR")
    val outputDir: String = getArgOrEnv(args, 1, "REST_CONSUMER_OUTPUT_DIR")
    val   doneDir: String = getArgOrEnv(args, 2, "REST_CONSUMER_DONE_DIR")

    val documentDir: String = getArgOrEnv(args, 3, "DOCUMENT_DIR")
    val ontologyDir: String = getArgOrEnv(args, 4, "ONTOLOGY_DIR")
    val  readingDir: String = getArgOrEnv(args, 5, "READING_DIR")

    FileUtils.ensureDirsExist(inputDir, outputDir, doneDir, documentDir, ontologyDir, readingDir)
    loop {
      () => new RestConsumerLoopApp(inputDir, outputDir, doneDir,
          documentDir, ontologyDir, readingDir).thread
    }
  }
}

case class RestConsumerRequest(file: File, jValue: JValue, documentId: String, ontologyIds: List[String])
