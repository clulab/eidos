package org.clulab.wm.wmexchanger.wmconsumer

import org.apache.http.HttpHost
import org.apache.http.client.methods.HttpGet
import org.apache.http.client.utils.URIBuilder
import org.apache.http.impl.client.CloseableHttpClient
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.Sourcer
import org.clulab.wm.eidoscommon.utils.StringUtils
import org.clulab.wm.wmexchanger.utils.RestExchanger
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods

import java.io.File
import java.net.URL
import scala.io.Source

class RealRestConsumer(service: String, username: String, password: String, annotations: Boolean = false)
    extends RestExchanger(service, username, password) with RestConsumerish {

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

  def download(file: File): String = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    val docId = StringUtils.beforeLast(file.getName, '.')
    val json = FileUtils.getTextFromFile(file)
    val jValue = JsonMethods.parse(json)
    val dateOpt = (jValue \ "release-date").extractOpt[String]
    val cdr = download(docId, dateOpt, annotations, closeableHttpClientOpt.get, httpHost)

    cdr
  }
}
