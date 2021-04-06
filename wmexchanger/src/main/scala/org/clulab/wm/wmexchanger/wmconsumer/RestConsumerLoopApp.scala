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
import org.clulab.wm.eidoscommon.utils.Logging
import org.clulab.wm.eidoscommon.utils.PropertiesBuilder
import org.clulab.wm.wmexchanger.utils.SafeThread
import org.clulab.wm.eidoscommon.utils.Sinker
import org.clulab.wm.eidoscommon.utils.Sourcer
import org.clulab.wm.eidoscommon.utils.StringUtils
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods
import org.slf4j.{Logger, LoggerFactory}

import java.io.File
import java.net.URL
import java.util.Properties
import scala.io.Source

// See https://hc.apache.org/httpcomponents-client-ga/tutorial/html/authentication.html
// and https://mkyong.com/java/apache-httpclient-basic-authentication-examples/
class RestConsumerLoopApp(args: Array[String]) {

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

  val inputDir: String = args(0)
  val outputDir: String = args(1)
  val doneDir: String = args(2)

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

      val newFile = FileEditor(file).setDir(doneDir).get

      file.renameTo(newFile)
    }
    catch {
      case exception: Exception =>
        RestConsumerLoopApp.logger.error(s"Exception for file $file", exception)
    }
  }

  val thread: SafeThread = new SafeThread(RestConsumerLoopApp.logger) {

    override def runSafely(): Unit = {
      //val closeableHttpClient = newCloseableHttpClient(url, username, password)

      // autoClose isn't executed if the thread is shot down, so this hook is used instead.
      //sys.ShutdownHookThread { closeableHttpClient.close() }

      while (!isInterrupted) {
        // Parallelize this
        val files = FileUtils.findFiles(inputDir, "json")

        // If there are files, make sure there is a connection
        // If not, don't even open the connection.
        files.foreach { file =>
//          processFile(closeableHttpClient, file)
          println(s"Processing ${file.getName}")

          try {
            val sourcer = Sourcer.sourceFromFile(file)
            val text = sourcer.mkString
            println(text)
            sourcer.close()

            val newFile = FileEditor(file).setDir(doneDir).get
            file.renameTo(newFile)

          }
          catch {
            case _: Throwable => println("It didn't open.")
          }
        }
        Thread.sleep(pauseDuration)
      }
    }
  }

  if (interactive)
    thread.waitSafely(waitDuration)
}

object RestConsumerLoopApp extends App with Logging {
  args.foreach(println)
  println("Done!")
  new RestConsumerLoopApp(args)
}
