package org.clulab.wm.wmexchanger.wmproducer

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.apache.http.HttpHeaders
import org.apache.http.HttpHost
import org.apache.http.auth.AuthScope
import org.apache.http.auth.UsernamePasswordCredentials
import org.apache.http.client.CredentialsProvider
import org.apache.http.client.methods.HttpPost
import org.apache.http.entity.ContentType
import org.apache.http.entity.mime.MultipartEntityBuilder
import org.apache.http.entity.mime.content.FileBody
import org.apache.http.entity.mime.content.StringBody
import org.apache.http.impl.client.BasicCredentialsProvider
import org.apache.http.impl.client.CloseableHttpClient
import org.apache.http.impl.client.HttpClientBuilder
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.PropertiesBuilder
import org.clulab.wm.eidoscommon.utils.Sourcer
import org.clulab.wm.eidoscommon.utils.StringUtils
import org.clulab.wm.wmexchanger.utils.Extensions
import org.clulab.wm.wmexchanger.utils.LockUtils
import org.clulab.wm.wmexchanger.utils.LoopApp
import org.clulab.wm.wmexchanger.utils.SafeThread
import org.clulab.wm.wmexchanger.wmconsumer.RestConsumerLoopApp
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods

import java.io.File
import java.net.URL
import java.util.Properties
import scala.io.Source

// See https://hc.apache.org/httpcomponents-client-ga/tutorial/html/authentication.html
// and https://mkyong.com/java/apache-httpclient-basic-authentication-examples/
// and https://stackoverflow.com/questions/2304663/apache-httpclient-making-multipart-form-post
class RestProducerLoopApp(inputDir: String, doneDir: String) {
  val useReal = true

  val version = "1.1.0"

  val config: Config = ConfigFactory.load("restproducer")
  val service: String = config.getString("RestProducerApp.service")
  val login: String = config.getString("RestProducerApp.login")
  val interactive: Boolean = config.getBoolean("RestProducerApp.interactive")
  val waitDuration: Int = config.getInt("RestProducerApp.duration.wait")
  val pauseDuration: Int = config.getInt("RestProducerApp.duration.pause")
  val properties: Properties = PropertiesBuilder.fromFile(login).get
  val username: String = properties.getProperty("username")
  val password: String = properties.getProperty("password")

  val thread: SafeThread = new SafeThread(RestConsumerLoopApp.logger, interactive, waitDuration) {

    def processFile(restProducer: RestProducerish, file: File): Unit = {
      try {
        RestProducerLoopApp.logger.info(s"Uploading ${file.getName}")
        val storageKey = restProducer.upload(file)

        RestProducerLoopApp.logger.info(s"Reporting storage key $storageKey for ${file.getName}")

        val doneFile = FileEditor(file).setDir(doneDir).get
        if (doneFile.exists) doneFile.delete
        file.renameTo(doneFile)
      }
      catch {
        case exception: Exception =>
          RestProducerLoopApp.logger.error(s"Exception for file $file", exception)
      }
    }

    override def runSafely(): Unit = {
      val restProducer =
          if (useReal) new RealRestProducer(service, username, password, version)
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

object RestProducerLoopApp extends App with LoopApp {
  val inputDir = args(0)
  val doneDir = args(1)

  loop {
    () => new RestProducerLoopApp(inputDir, doneDir).thread
  }
}
