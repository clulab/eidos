package org.clulab.wm.wmexchanger2.wmconsumer

import org.apache.http.HttpHost
import org.apache.http.client.methods.HttpGet
import org.apache.http.client.utils.URIBuilder
import org.apache.http.impl.client.CloseableHttpClient
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.Sourcer
import org.clulab.wm.wmexchanger.utils.RestExchanger
import org.json4s.DefaultFormats
import org.json4s.JValue
import org.json4s.jackson.JsonMethods

import java.net.URL
import scala.io.Source

class RealRestOntologyConsumer(service: String, username: String, password: String)
    extends RestExchanger(service, username, password) with RestConsumerish {

  def newHttpGet(url: URL, ontologyId: String): HttpGet = {
    val uriBuilder = new URIBuilder(url.toURI)
    uriBuilder.addParameter("id", ontologyId)

    val uri = uriBuilder.toString
    val httpGet = new HttpGet(uri)

    // This is a poor person's substitute for preemptive basic authentication.
    // val credentials = new UsernamePasswordCredentials(username, password)
    // httpGet.addHeader(new BasicScheme().authenticate(credentials, httpGet))

    httpGet
  }

  def download(ontologyId: String, closeableHttpClient: CloseableHttpClient, httpHost: HttpHost): String = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    val httpGet = newHttpGet(url, ontologyId)
    val ontology = closeableHttpClient.execute(httpHost, httpGet).autoClose { response =>
      val statusCode = response.getStatusLine.getStatusCode

      if (statusCode != 200)
        throw new Exception(s"Status code '$statusCode' for ontologyId '$ontologyId''")

      val content = response.getEntity.getContent
      val json = Source.fromInputStream(content, Sourcer.utf8).autoClose { source =>
        source.mkString
      }
      val jValue = JsonMethods.parse(json)

      (jValue \ "ontology").extract[String]
    }
    ontology
  }

  override def download(ontologyId: String, jValueOpt: Option[JValue] = None): String = {
    download(ontologyId, closeableHttpClientOpt.get, httpHost)
  }
}
