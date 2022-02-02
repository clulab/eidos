package org.clulab.wm.wmexchanger.utils

import org.apache.http.HttpHost
import org.apache.http.auth.AuthScope
import org.apache.http.auth.UsernamePasswordCredentials
import org.apache.http.client.CredentialsProvider
import org.apache.http.impl.client.BasicCredentialsProvider
import org.apache.http.impl.client.CloseableHttpClient
import org.apache.http.impl.client.HttpClientBuilder

import java.net.URL

// See https://hc.apache.org/httpcomponents-client-ga/tutorial/html/authentication.html
// and https://mkyong.com/java/apache-httpclient-basic-authentication-examples/
// and https://stackoverflow.com/questions/2304663/apache-httpclient-making-multipart-form-post
class RestExchanger(service: String, username: String, password: String) {
  protected val url = new URL(service)
  protected val httpHost: HttpHost = newHttpHost(url)

  // Keep this closed by default and only open when needed.
  protected var closeableHttpClientOpt: Option[CloseableHttpClient] = None

  def open(): Unit = {
    if (closeableHttpClientOpt.isEmpty) {
      val closeableHttpClient = newCloseableHttpClient(url, username, password)
      closeableHttpClientOpt = Some(closeableHttpClient)
    }
  }

  def close(): Unit = {
    closeableHttpClientOpt.foreach { closeableHttpClient =>
      closeableHttpClient.close()
      closeableHttpClientOpt = None
    }
  }

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

  def newCloseableHttpClient(url: URL, userName: String, password: String): CloseableHttpClient = {
    val closeableHttpClient = HttpClientBuilder.create

    if (userName.nonEmpty)
      closeableHttpClient.setDefaultCredentialsProvider(newCredentialsProvider(url, userName, password))
    closeableHttpClient.build()
  }
}
