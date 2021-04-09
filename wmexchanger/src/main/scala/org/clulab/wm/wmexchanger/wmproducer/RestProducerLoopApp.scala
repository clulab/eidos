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
  val version = "1.1.0"

  def getPort(url: URL): Int = {
    val explicitPort = url.getPort

    if (explicitPort >= 0)
      explicitPort
    else if (url.getProtocol == "https")
      443
    else 80
  }

  def newCredentialsProvider(url: URL, username: String, password: String): CredentialsProvider = {
    val credentialsProvider = {
      val authScope = new AuthScope(url.getHost, getPort(url))
      val credentials = new UsernamePasswordCredentials(username, password)
      val credentialsProvider = new BasicCredentialsProvider

      credentialsProvider.setCredentials(authScope, credentials)
      credentialsProvider
    }

    credentialsProvider
  }

  def newHttpHost(url: URL): HttpHost = {
    val hostName = url.getHost
    val protocol = url.getProtocol
    val port = getPort(url)
    val httpHost = new HttpHost(hostName, port, protocol)

    httpHost
  }

  def newHttpPost(url: URL, metadata: String, file: File): HttpPost = {
    val stringBody = new StringBody(metadata, ContentType.TEXT_PLAIN)
    val fileBody = new FileBody(file)
    val httpEntity = MultipartEntityBuilder
        .create
        .addPart("metadata", stringBody)
        .addPart("file", fileBody)
        .build
    val httpPost = new HttpPost(url.toURI)

    httpPost.setEntity(httpEntity)
    httpPost.setHeader(HttpHeaders.EXPECT, "100-continue")
    httpPost.expectContinue()
    httpPost
  }

  def newCloseableHttpClient(url: URL, userName: String, password: String): CloseableHttpClient = {
    val credentialsProvider = newCredentialsProvider(url, userName, password)
    val closeableHttpClient = HttpClientBuilder
        .create
        .setDefaultCredentialsProvider(credentialsProvider)
        .build

    closeableHttpClient
  }

  def upload(docId: String, metadata: String, file: File, closeableHttpClient: CloseableHttpClient, httpHost: HttpHost): String = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    val httpPost = newHttpPost(url, metadata, file)
    val storageKey = closeableHttpClient.execute(httpHost, httpPost).autoClose { response =>
      val statusCode = response.getStatusLine.getStatusCode

      if (statusCode != 201)
        throw new Exception(s"Status code '$statusCode' for docId '$docId''")

      val content = response.getEntity.getContent
      val storageKey = Source.fromInputStream(content, Sourcer.utf8).autoClose { source =>
        val json = source.mkString
        val jValue = JsonMethods.parse(json)
        val storageKey = (jValue \ "storage_key").extract[String]

        storageKey
      }

      storageKey
    }
    storageKey
  }

  def upload(file: File, closeableHttpClient: CloseableHttpClient, httpHost: HttpHost): String = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    val docId = StringUtils.beforeLast(file.getName, '.')
    val metadata = s"""{ "identity": "eidos", "version": "$version", "document_id": "$docId" }"""
    val storageKey = upload(docId, metadata, file, closeableHttpClient, httpHost)

    storageKey
  }

  val config: Config = ConfigFactory.load("restproducer")
  val service: String = config.getString("RestProducerApp.service")
  val login: String = config.getString("RestProducerApp.login")
  val interactive: Boolean = config.getBoolean("RestProducerApp.interactive")
  val waitDuration: Int = config.getInt("RestProducerApp.duration.wait")
  val pauseDuration: Int = config.getInt("RestProducerApp.duration.pause")
  val properties: Properties = PropertiesBuilder.fromFile(login).get
  val username: String = properties.getProperty("username")
  val password: String = properties.getProperty("password")

  val url = new URL(service)
  val httpHost: HttpHost = newHttpHost(url)

  def processFile(closeableHttpClient: CloseableHttpClient, file: File): Unit = {
    try {
      RestProducerLoopApp.logger.info(s"Uploading ${file.getName}")
      val storageKey = upload(file, closeableHttpClient, httpHost)

      RestProducerLoopApp.logger.info(s"Reporting storage key $storageKey for ${file.getName}")

      val doneFile = FileEditor(file).setDir(doneDir).get
      file.renameTo(doneFile)
    }
    catch {
      case exception: Exception =>
        RestProducerLoopApp.logger.error(s"Exception for file $file", exception)
    }
  }

  val thread: SafeThread = new SafeThread(RestConsumerLoopApp.logger, interactive, waitDuration) {
    // Keep this closed by default and only open when needed.
    var closeableHttpClientOpt: Option[CloseableHttpClient] = None

    def open(): CloseableHttpClient = {
      closeableHttpClientOpt.getOrElse {
        val closeableHttpClient = newCloseableHttpClient(url, username, password)
        closeableHttpClientOpt = Some(closeableHttpClient)
        closeableHttpClient
      }
    }

    def close(): Unit = {
      closeableHttpClientOpt.foreach { closeableHttpClient =>
        closeableHttpClient.close()
        closeableHttpClientOpt = None
      }
    }

    // autoClose isn't executed if the thread is shot down, so this hook is included just in case.
    sys.ShutdownHookThread {
      close()
    }

    override def runSafely(): Unit = {
      while (!isInterrupted) {
        val files = LockUtils.findFiles(inputDir, Extensions.jsonld, Extensions.lock)

        if (files.nonEmpty) {
          val closeableHttpClient = open()

          files.par.foreach { file =>
            processFile(closeableHttpClient, file)
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

object RestProducerLoopApp extends App with LoopApp {
  val inputDir = args(0)
  val doneDir = args(1)

  loop {
    () => new RestProducerLoopApp(inputDir, doneDir).thread
  }
}
