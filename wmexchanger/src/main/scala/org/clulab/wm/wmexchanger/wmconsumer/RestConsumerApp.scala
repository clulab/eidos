package org.clulab.wm.wmexchanger.wmconsumer

import java.io.File
import java.net.URL

import com.typesafe.config.ConfigFactory
import org.apache.http.HttpHost
import org.apache.http.auth.AuthScope
import org.apache.http.auth.UsernamePasswordCredentials
import org.apache.http.client.CredentialsProvider
import org.apache.http.client.methods.HttpGet
import org.apache.http.client.utils.URIBuilder
import org.apache.http.impl.client.BasicCredentialsProvider
import org.apache.http.impl.client.CloseableHttpClient
import org.apache.http.impl.client.HttpClientBuilder
import org.clulab.wm.eidoscommon.utils.{FileEditor, FileUtils, PropertiesBuilder, Sinker, Sourcer, StringUtils}
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.Logging
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods

import scala.io.Source

// See https://hc.apache.org/httpcomponents-client-ga/tutorial/html/authentication.html
// and https://mkyong.com/java/apache-httpclient-basic-authentication-examples/
object RestConsumerApp extends App with Logging {

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

  def newHttpGet(url: URL, docId: String, date: String, annotations: Boolean): HttpGet = {
    val uriBuilder = new URIBuilder(url.toURI)
    val pathSegments = {
      val pathSegments = uriBuilder.getPathSegments

      pathSegments.add(docId)
      pathSegments
    }
    val uri = uriBuilder
        .setPathSegments(pathSegments)
        .addParameter("date", date)
        .addParameter("annotations", annotations.toString)
        .toString
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

  def download(docId: String, date: String, annotations: Boolean, closeableHttpClient: CloseableHttpClient, httpHost: HttpHost): String = {
    val httpGet = newHttpGet(url, docId, date, annotations)
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
    val date = (jValue \ "release-date").extract[String]
    val cdr = download(docId, date, annotations, closeableHttpClient, httpHost)

    cdr
  }

  val inputDir = args(0)
  val outputDir = args(1)
  val doneDir = args(2)

  val config = ConfigFactory.load("restconsumer")
  val service = config.getString("RestConsumerApp.service")
  val annotations = config.getBoolean("RestConsumerApp.annotations")
  val login = config.getString("RestConsumerApp.login")
  val properties = PropertiesBuilder.fromFile(login).get
  val username = properties.getProperty("username")
  val password = properties.getProperty("password")

  val url = new URL(service)
  val httpHost = newHttpHost(url)

  newCloseableHttpClient(url, username, password).autoClose { closeableHttpClient =>
    val files = FileUtils.findFiles(inputDir, "json")

    files.foreach { file =>
      try {
        logger.info(s"Downloading ${file.getName}")
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
          logger.error(s"Exception for file $file", exception)
      }
    }
  }
}
