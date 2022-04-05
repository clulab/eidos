package org.clulab.wm.wmexchanger2.wmconsumer

import org.apache.http.HttpHost
import org.apache.http.auth.UsernamePasswordCredentials
import org.apache.http.client.methods.HttpGet
import org.apache.http.client.utils.URIBuilder
import org.apache.http.impl.auth.BasicScheme
import org.apache.http.impl.client.CloseableHttpClient
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.Sourcer
import org.clulab.wm.wmexchanger.utils.RestExchanger
import org.json4s.DefaultFormats
import org.json4s.JValue

import java.net.URL
import scala.io.Source

class RealRestDocumentConsumer(service: String, username: String, password: String, annotations: Boolean = false)
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

    // This is a poor person's substitute for preemptive basic authentication.
    val credentials = new UsernamePasswordCredentials(username, password)
    httpGet.addHeader(new BasicScheme().authenticate(credentials, httpGet))

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

  def download(docId: String, jValueOpt: Option[JValue] = None): String = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    val dateOpt = jValueOpt.flatMap { jValue =>
      (jValue \ "document" \ "release-date").extractOpt[String]
    }
    val cdr = download(docId, dateOpt, annotations, closeableHttpClientOpt.get, httpHost)

    cdr
  }
}
