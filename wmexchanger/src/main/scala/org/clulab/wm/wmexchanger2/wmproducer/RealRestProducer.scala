package org.clulab.wm.wmexchanger2.wmproducer

import org.apache.http.HttpHeaders
import org.apache.http.HttpHost
import org.apache.http.auth.UsernamePasswordCredentials
import org.apache.http.client.methods.HttpPost
import org.apache.http.entity.ContentType
import org.apache.http.entity.mime.MultipartEntityBuilder
import org.apache.http.entity.mime.content.FileBody
import org.apache.http.entity.mime.content.StringBody
import org.apache.http.impl.auth.BasicScheme
import org.apache.http.impl.client.CloseableHttpClient
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.Sourcer
import org.clulab.wm.eidoscommon.utils.StringUtils
import org.clulab.wm.wmexchanger.utils.RestExchanger
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods

import java.io.File
import java.net.URL
import scala.io.Source

// See https://hc.apache.org/httpcomponents-client-ga/tutorial/html/authentication.html
// and https://mkyong.com/java/apache-httpclient-basic-authentication-examples/
// and https://stackoverflow.com/questions/2304663/apache-httpclient-making-multipart-form-post
class RealRestProducer(service: String, username: String, password: String, eidosVersion: String, ontologyVersion: String)
    extends RestExchanger(service, username, password) with RestProducerish {

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

    // This is a poor person's substitute for preemptive basic authentication.
    val credentials = new UsernamePasswordCredentials(username, password)
    httpPost.addHeader(new BasicScheme().authenticate(credentials, httpPost))

    httpPost.expectContinue()
    httpPost
  }

  def upload(documentId: String, metadata: String, file: File, closeableHttpClient: CloseableHttpClient, httpHost: HttpHost): String = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    val httpPost = newHttpPost(url, metadata, file)
    val storageKey = closeableHttpClient.execute(httpHost, httpPost).autoClose { response =>
      val statusCode = response.getStatusLine.getStatusCode

      if (statusCode != 201)
        throw new Exception(s"Status code '$statusCode' for docId '$documentId''")

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

  def upload(file: File, documentId: String, ontologyId: String): String = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    val metadata = s"""{ "identity": "eidos", "version": "$eidosVersion", "document_id": "$documentId", "output_version": "$ontologyId" }"""
    val storageKey = upload(documentId, metadata, file, closeableHttpClientOpt.get, httpHost)

    storageKey
  }
}
