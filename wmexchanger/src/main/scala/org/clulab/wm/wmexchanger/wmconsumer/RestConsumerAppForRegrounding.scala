package org.clulab.wm.wmexchanger.wmconsumer

import com.typesafe.config.{Config, ConfigFactory}
import org.apache.http.HttpHost
import org.apache.http.auth.{AuthScope, UsernamePasswordCredentials}
import org.apache.http.client.CredentialsProvider
import org.apache.http.client.methods.HttpGet
import org.apache.http.client.utils.URIBuilder
import org.apache.http.impl.client.{BasicCredentialsProvider, CloseableHttpClient, HttpClientBuilder}
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils._
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods

import java.io.File
import java.net.URL
import java.nio.file.{Files, Paths}
import scala.io.Source

// See https://hc.apache.org/httpcomponents-client-ga/tutorial/html/authentication.html
// and https://mkyong.com/java/apache-httpclient-basic-authentication-examples/
object RestConsumerAppForRegrounding extends App with Logging {

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

  // The input directory comes from the output of Kafka, which hasn't changed.
  val inputDir = args(0)
  // This is the standard one for reading.  If we haven't already read it, but it here.
  val readingOutputDir = args(1)
  // Here is the one for regrounding.  Put files here if they have already been read.
  val regroundingOutputDir = args(2)
  // This is usually a subdirectory of the Kafka input directory.
  val doneDir = args(3)

  // These should be the dirs of the actual (pre)readings because the files need to
  // be copied to the regroundingOutputDir.  For bookkeeping purposes, they will still
  // be moved to a local done dir.  This way one can observe how many have been included.
  val prereadInputDirs = Seq(
    // The ordering is significant.
    "../corpora/julyEmbed/41950/output",
    "../corpora/julyEmbed/61538/output" // possibly 1801, ata, 1366
  )
  val prereadDoneDirs = prereadInputDirs.map(_ + "/done")
  val prereadFiles = prereadInputDirs.map { prereadInputDir =>
    FileUtils.findFiles(prereadInputDir, "jsonld")
  }

  val config: Config = ConfigFactory.defaultApplication().resolve()
  val service = config.getString("rest.consumer.service")
  val annotations = config.getBoolean("rest.consumer.annotations")
  val username: String = config.getString("rest.consumer.username")
  val password: String = config.getString("rest.consumer.password")

  val url = new URL(service)
  val httpHost = newHttpHost(url)

  newCloseableHttpClient(url, username, password).autoClose { closeableHttpClient =>
    val files = FileUtils.findFiles(inputDir, "json")

    files.foreach { file =>
      try {
        val prereadFilename = FileEditor(file).setExt("jsonld").get.getName
        val index = prereadFiles.indices.find { index =>
          prereadFiles(index).exists { file =>
            file.getName == prereadFilename
          }
        }.getOrElse(-1)
        val reground = index >= 0

        if (reground) {
          logger.info(s"Arranging to reground ${file.getName}")
          val prereadFile = prereadFiles(index).find(_.getName == prereadFilename).get
          val copyFile = FileEditor(prereadFile).setDir(regroundingOutputDir).get
          Files.copy(prereadFile.toPath, copyFile.toPath)
          val prereadDoneFile = FileEditor(prereadFile).setDir(prereadDoneDirs(index)).get
          FileUtils.rename(prereadFile, prereadDoneFile)
        }
        else {
          logger.info(s"Downloading ${file.getName}")
          val cdr = download(file, annotations, closeableHttpClient, httpHost)
          val outputFile = FileEditor(file).setDir(readingOutputDir).get

          Sinker.printWriterFromFile(outputFile, append = false).autoClose { printWriter =>
            printWriter.print(cdr)
          }
        }
        val doneFile = FileEditor(file).setDir(doneDir).get
        FileUtils.rename(file, doneFile)
      }
      catch {
        case exception: Exception =>
          logger.error(s"Exception for file $file", exception)
      }
    }
  }
}
