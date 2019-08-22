package org.clulab.wm.elasticsearch.apps

import java.io.File
import java.net.URL
import java.nio.file.Files
import java.nio.file.Paths
import java.util

import com.amazonaws.auth.AWS4Signer
import com.amazonaws.auth.AWSCredentialsProvider
import com.amazonaws.auth.profile.ProfileCredentialsProvider
import com.amazonaws.http.AWSRequestSigningApacheInterceptor
import com.amazonaws.services.s3.AmazonS3
import com.amazonaws.services.s3.AmazonS3ClientBuilder
import com.amazonaws.services.s3.model.GetObjectRequest
import org.apache.http.HttpHost
import org.apache.http.HttpRequestInterceptor
import org.clulab.wm.elasticsearch.utils.Closer.AutoCloser
import org.clulab.wm.elasticsearch.utils.Sinker
import org.clulab.wm.elasticsearch.utils.StringUtils
import org.elasticsearch.action.search.ClearScrollRequest
import org.elasticsearch.action.search.SearchRequest
import org.elasticsearch.action.search.SearchResponse
import org.elasticsearch.action.search.SearchScrollRequest
import org.elasticsearch.client.RequestOptions
import org.elasticsearch.client.RestClient
import org.elasticsearch.client.RestHighLevelClient
import org.elasticsearch.common.unit.TimeValue
import org.elasticsearch.rest.RestStatus
import org.elasticsearch.script.ScriptType
import org.elasticsearch.script.mustache.SearchTemplateRequest
import org.elasticsearch.search.SearchHit
import org.elasticsearch.search.aggregations.bucket.terms.ParsedStringTerms
import org.elasticsearch.search.aggregations.bucket.terms.Terms
import org.json4s._
import org.json4s.jackson.JsonMethods
import org.slf4j.Logger
import org.slf4j.LoggerFactory

object ElasticSearch4Dart extends App {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val indexName = "cdr_search"
  val resultsPerQuery = 50
  val timeout = resultsPerQuery * 2 // seconds

  // See https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-request-signing.html#es-request-signing-java
  // and https://github.com/WorldModelers/Document-Schema/blob/master/Document-Retrieval.ipynb
  def newRestHighLevelClient(): RestHighLevelClient = {
    val serviceEndpoint = "localhost"
    val port = 9200
    val scheme = "http"
    val httpHost = new HttpHost(serviceEndpoint, port, scheme)
    val restClientBuilder = RestClient.builder(httpHost)

    new RestHighLevelClient(restClientBuilder)
  }

  protected def newSearchTemplateRequest(script: String) = {
    val searchTemplateRequest = new SearchTemplateRequest()
    val searchRequest = new SearchRequest(indexName)
    val timeValue = TimeValue.timeValueSeconds(timeout)

    searchRequest.scroll(timeValue)
    searchTemplateRequest.setRequest(searchRequest)
    searchTemplateRequest.setScriptType(ScriptType.INLINE)
    searchTemplateRequest.setScript(script)
    searchTemplateRequest.setScriptParams(new util.HashMap[String, AnyRef])
    searchTemplateRequest
  }

  protected def writeFile(text: String, fileDir: String, id: String, fileType: String): Unit = {
    val filename = fileDir + File.separatorChar + id + fileType

    Sinker.printWriterFromFile(filename).autoClose { printWriter =>
      printWriter.println(text)
    }
  }

  def downloadSearch(searchOpt: Option[String], metaDir: String, textDir: String): Unit = {
    newRestHighLevelClient().autoClose { restHighLevelClient =>
      val jsonSearchOpt = searchOpt.map { search => JsonMethods.pretty(new JString(search)) }
      // Use "term" for exact matches, "match" for fuzzy matches.
      val script = jsonSearchOpt.map { jsonSearch =>
        s"""
          |{
          |  "size" : $resultsPerQuery,
          |  "query" : {
          |    "match" : {
          |      "extracted_text" : $jsonSearch
          |    }
          |  }
          |}
        """}.getOrElse(
        s"""
           |{
           |  "size" : $resultsPerQuery,
           |  "query" : {
           |    "match_all": {}
           |  }
           |}
        """.stripMargin
      ).stripMargin
      val searchTemplateRequest = newSearchTemplateRequest(script)
      val requestOptions = RequestOptions.DEFAULT
      val searchTemplateResponse = restHighLevelClient.searchTemplate(searchTemplateRequest, requestOptions)
      val status = searchTemplateResponse.status

      if (status != RestStatus.OK)
        throw new RuntimeException(s"Bad status in searchTemplateResponse: $searchTemplateResponse")

      var hits = searchTemplateResponse.getResponse.getHits

      if (hits.totalHits > 0) {
        var scrollIdOpt = Option(searchTemplateResponse.getResponse.getScrollId)
        var continue = true

        do {
          hits.forEach { hit: SearchHit =>
            implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

            val id = hit.getId
            val metaText = hit.toString
            val json = hit.getSourceAsString
            val jValue = JsonMethods.parse(json)
            val extractedText = (jValue \ "extracted_text").extract[String]

            writeFile(metaText, metaDir, id, ".json")
            writeFile(extractedText, textDir, id, ".txt")
          }

          continue = scrollIdOpt.exists { scrollId =>
            // See https://www.elastic.co/guide/en/elasticsearch/client/java-rest/master/java-rest-high-search-scroll.html
            val searchScrollRequest = new SearchScrollRequest(scrollId)

            searchScrollRequest.scroll(TimeValue.timeValueSeconds(timeout)) // It is important to rescroll the scroll!

            val searchResponse: SearchResponse = restHighLevelClient.scroll(searchScrollRequest, RequestOptions.DEFAULT)

            scrollIdOpt = Option(searchResponse.getScrollId)
            hits = searchResponse.getHits
            hits.getHits.length > 0
          }
        } while(continue)

        scrollIdOpt.foreach { scrollId =>
          val clearScrollRequest = new ClearScrollRequest()

          clearScrollRequest.addScrollId(scrollId)
          restHighLevelClient.clearScroll(clearScrollRequest, RequestOptions.DEFAULT)
        }
      }
    }
  }

  val metaDirOpt = args.lift(0)
  val textDirOpt = args.lift(1)
  val searchOpt = args.lift(2)

  if (metaDirOpt.isDefined && textDirOpt.isDefined) {
    val metaDir = metaDirOpt.get
    val textDir = textDirOpt.get

    Files.createDirectories(Paths.get(metaDir))
    Files.createDirectories(Paths.get(textDir))
    downloadSearch(searchOpt, metaDir, textDir)
  }
  else
    println("argumets: metaDir textDir [searchText]")
}
