package org.clulab.wm.wmexchanger.wmconsumer

import com.typesafe.config.{Config, ConfigFactory}
import org.apache.http.HttpHost
import org.apache.http.auth.{AuthScope, UsernamePasswordCredentials}
import org.apache.http.client.CredentialsProvider
import org.apache.http.client.methods.HttpGet
import org.apache.http.client.utils.URIBuilder
import org.apache.http.impl.client.{BasicCredentialsProvider, CloseableHttpClient, HttpClientBuilder}
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.PropertiesBuilder
import org.clulab.wm.wmexchanger.utils.SafeThread
import org.clulab.wm.eidoscommon.utils.Sinker
import org.clulab.wm.eidoscommon.utils.Sourcer
import org.clulab.wm.eidoscommon.utils.StringUtils
import org.clulab.wm.wmexchanger.utils.Extensions
import org.clulab.wm.wmexchanger.utils.LockUtils
import org.clulab.wm.wmexchanger.utils.LoopApp
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods

import java.io.File
import java.net.URL
import java.util.Properties
import scala.io.Source

// See https://hc.apache.org/httpcomponents-client-ga/tutorial/html/authentication.html
// and https://mkyong.com/java/apache-httpclient-basic-authentication-examples/
class RestConsumerLoopApp(inputDir: String, outputDir: String, doneDir: String) {

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

  def newHttpGet(url: URL, docId: String, dateOpt: Option[String], annotations: Boolean): HttpGet = {
    val uriBuilder = new URIBuilder(url.toURI)
    val pathSegments = {
      val pathSegments = uriBuilder.getPathSegments

      pathSegments.add(docId)
      pathSegments
    }

    uriBuilder.setPathSegments(pathSegments)
    dateOpt.foreach { date => uriBuilder.addParameter("date", date) }
    uriBuilder.addParameter("annotations", annotations.toString)

    val uri = uriBuilder.toString
    val httpGet = new HttpGet(uri)

    httpGet
  }

  def newCloseableHttpClient(url: URL, userName: String, password: String): CloseableHttpClient = {
    val credentialsProvider = newCredentialsProvider(url, userName, password)
    val closeableHttpClient = HttpClientBuilder
        .create
        .setDefaultCredentialsProvider(credentialsProvider)
        .build

    closeableHttpClient
  }

  def download(docId: String, dateOpt: Option[String], annotations: Boolean, closeableHttpClient: CloseableHttpClient, httpHost: HttpHost): String = {
    val httpGet = newHttpGet(url, docId, dateOpt, annotations)
    val cdr = closeableHttpClient.execute(httpHost, httpGet).autoClose { response =>
      val statusCode = response.getStatusLine.getStatusCode

      if (statusCode != 200)
        throw new Exception(s"Status code '$statusCode' for docId '$docId''")

      val content = response.getEntity.getContent
      val cdr = Source.fromInputStream(content, Sourcer.utf8).autoClose { source =>
        source.mkString
      }

      cdr
    }
    cdr
  }

  def download(file: File, annotations: Boolean, closeableHttpClient: CloseableHttpClient, httpHost: HttpHost): String = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    val docId = StringUtils.beforeLast(file.getName, '.')
    val json = FileUtils.getTextFromFile(file)
    val jValue = JsonMethods.parse(json)
    val dateOpt = (jValue \ "release-date").extractOpt[String]
    val cdr = download(docId, dateOpt, annotations, closeableHttpClient, httpHost)

    cdr
  }

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

  val url = new URL(service)
  val httpHost: HttpHost = newHttpHost(url)

  def processFile(closeableHttpClient: CloseableHttpClient, file: File): Unit = {
    try {
      RestConsumerLoopApp.logger.info(s"Downloading ${file.getName}")
      val cdr = download(file, annotations, closeableHttpClient, httpHost)
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

  val thread: SafeThread = new SafeThread(RestConsumerLoopApp.logger, interactive, waitDuration) {
    // Keep this closed by default and only open when needed.
    var closeableHttpClientOpt: Option[CloseableHttpClient] = None
    // autoClose isn't executed if the thread is shot down, so this hook is used instead.

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
      closeableHttpClientOpt.foreach(_.close())
    }

    override def runSafely(): Unit = {
      while (!isInterrupted) {
        LockUtils.cleanupLocks(outputDir, Extensions.lock, Extensions.json)

        val files = LockUtils.findFiles(inputDir, Extensions.json, Extensions.lock).par

        if (files.nonEmpty) {
          val closeableHttpClient = open()

          files.foreach { file =>
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

object RestConsumerLoopApp extends App with LoopApp {
  val inputDir: String = args(0)
  val outputDir: String = args(1)
  val doneDir: String = args(2)

  loop {
    () => new RestConsumerLoopApp(inputDir, outputDir, doneDir).thread
  }
}
