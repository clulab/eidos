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

import java.net.URL
import scala.io.Source

class RestOntologyConsumer(service: String, username: String, password: String)
    extends RestExchanger(service, username, password) with RestConsumerish {

  def newHttpGet(url: URL, ontologyId: String): HttpGet = {
    val uriBuilder = new URIBuilder(url.toURI)
    uriBuilder.addParameter("id", ontologyId)

    val uri = uriBuilder.toString
    val httpGet = new HttpGet(uri)

    httpGet
  }

  def download(ontologyId: String, closeableHttpClient: CloseableHttpClient, httpHost: HttpHost): String = {
    val httpGet = newHttpGet(url, ontologyId)
    val ontology = closeableHttpClient.execute(httpHost, httpGet).autoClose { response =>
      val statusCode = response.getStatusLine.getStatusCode

      if (statusCode != 200)
        throw new Exception(s"Status code '$statusCode' for ontologyId '$ontologyId''")

      val content = response.getEntity.getContent
      val ontology = Source.fromInputStream(content, Sourcer.utf8).autoClose { source =>
        source.mkString
      }

      ontology
    }
    ontology
  }

  override def download(ontologyId: String, jValue: JValue): String = {
    download(ontologyId, closeableHttpClientOpt.get, httpHost)
  }
}
