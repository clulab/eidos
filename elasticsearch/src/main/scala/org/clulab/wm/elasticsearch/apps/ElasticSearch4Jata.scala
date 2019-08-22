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
import org.elasticsearch.client.RestClient
import org.elasticsearch.client.RestHighLevelClient
import org.elasticsearch.client.RequestOptions
import org.elasticsearch.common.unit.TimeValue
import org.elasticsearch.rest.RestStatus
import org.elasticsearch.script.ScriptType
import org.elasticsearch.search.SearchHit
import org.elasticsearch.search.aggregations.bucket.terms.ParsedStringTerms
import org.elasticsearch.search.aggregations.bucket.terms.Terms
import org.elasticsearch.script.mustache.SearchTemplateRequest
import org.json4s._
import org.json4s.jackson.JsonMethods
import org.slf4j.Logger
import org.slf4j.LoggerFactory

object ElasticSearch4Jata extends App {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val indexName = "wm-dev"
  val resultsPerQuery = 50
  val timeout = resultsPerQuery * 2 // seconds

  // See https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-request-signing.html#es-request-signing-java
  // and https://github.com/WorldModelers/Document-Schema/blob/master/Document-Retrieval.ipynb
  def newRestHighLevelClient(): RestHighLevelClient = {
    val serviceEndpoint = "search-world-modelers-dev-gjvcliqvo44h4dgby7tn3psw74.us-east-1.es.amazonaws.com"
    val serviceName = "es"
    val regionName = "us-east-1"
    val profileName = "wmuser"
    val port = 443
    val scheme = "https"

    val aws4signer = {
      val signer = new AWS4Signer()
      signer.setServiceName(serviceName)
      signer.setRegionName(regionName)
      signer
    }
    val awsCredentialsProvider: AWSCredentialsProvider = new ProfileCredentialsProvider(profileName)
    val interceptor: HttpRequestInterceptor = new AWSRequestSigningApacheInterceptor(serviceName, aws4signer, awsCredentialsProvider)
    val httpHost = new HttpHost(serviceEndpoint, port, scheme)
    val restClientBuilder = RestClient
        .builder(httpHost)
        .setHttpClientConfigCallback(_.addInterceptorLast(interceptor))

    new RestHighLevelClient(restClientBuilder)
  }

  def newS3Client(): AmazonS3 = {
    val profileName = "wmuser"
    val regionName = "us-east-1"

    val awsCredentialsProvider: AWSCredentialsProvider = new ProfileCredentialsProvider(profileName)
    val amazonS3 = AmazonS3ClientBuilder.standard()
        .withCredentials(awsCredentialsProvider)
        .withRegion(regionName)
        .build()

    amazonS3
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

  protected def writeMeta(text: String, metaDir: String, id: String, fileType: String): Unit = {
    val filename = metaDir + File.separatorChar + id + fileType

    Sinker.printWriterFromFile(filename).autoClose { printWriter =>
      printWriter.println(text)
    }
  }

  protected def writeRaw(s3Client: AmazonS3, storedUrl: String, rawDir: String, id: String, fileType: String): Unit = {
    val filename = rawDir + File.separatorChar + id + fileType
    val bucketName = "world-modelers"
    val key = StringUtils.afterFirst(new URL(storedUrl).getFile, '/')
    val getObjectRequest = new GetObjectRequest(bucketName, key)

    s3Client.getObject(getObjectRequest, new File(filename))
  }

  def downloadCategory(category: String, metaDir: String, rawDir: String): Unit = {
    newRestHighLevelClient().autoClose { restHighLevelClient =>
      val jsonCategory = JsonMethods.pretty(new JString(category))
      // Use "term" for exact matches, "match" for fuzzy matches.
      val script =
        s"""
          |{
          |  "size" : $resultsPerQuery,
          |  "query" : {
          |    "term" : {
          |      "category.keyword" : {
          |        "value" : $jsonCategory
          |      }
          |    }
          |  }
          |}
        """.stripMargin
      val searchTemplateRequest = newSearchTemplateRequest(script)
      val requestOptions = RequestOptions.DEFAULT
      val searchTemplateResponse = restHighLevelClient.searchTemplate(searchTemplateRequest, requestOptions)
      val status = searchTemplateResponse.status

      if (status != RestStatus.OK)
        throw new RuntimeException(s"Bad status in searchTemplateResponse: $searchTemplateResponse")

      var hits = searchTemplateResponse.getResponse.getHits

      if (hits.totalHits > 0) {
        var scrollIdOpt = Option(searchTemplateResponse.getResponse.getScrollId)
        val s3Client = newS3Client()
        var continue = true

        do {
          hits.forEach { hit: SearchHit =>
            implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

            val id = hit.getId
            val text = hit.toString
            val json = hit.getSourceAsString
            val jValue = JsonMethods.parse(json)
            val fileType = (jValue \ "file_type").extract[String]
            val storedUrl = (jValue \ "stored_url").extract[String]

            writeMeta(text, metaDir, id, ".json")
            writeRaw(s3Client, storedUrl, rawDir, id, fileType)
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

  def listCategories(): Unit = {
    newRestHighLevelClient().autoClose { restHighLevelClient =>
      // For this see particularly https://www.elastic.co/guide/en/elasticsearch/client/java-rest/7.2/_search_apis.html
      val categories = "categories"
      val script =
        s"""
          |{
          |  "aggs" : {
          |    "$categories" : {
          |      "terms" : { "field" : "category.keyword" }
          |    }
          |  }
          |}
        """.stripMargin

      val searchTemplateRequest = newSearchTemplateRequest(script)
      val requestOptions = RequestOptions.DEFAULT
      val searchTemplateResponse = restHighLevelClient.searchTemplate(searchTemplateRequest, requestOptions)
      val status = searchTemplateResponse.status

      if (status != RestStatus.OK)
        throw new RuntimeException(s"Bad status in searchTemplateResponse: $searchTemplateResponse")

      val searchResponse: SearchResponse = searchTemplateResponse.getResponse
      val stringTerms: ParsedStringTerms = searchResponse.getAggregations.get[ParsedStringTerms](categories)
      val buckets: util.List[_ <: Terms.Bucket] = stringTerms.getBuckets

      println("key\tcount")
      buckets.forEach { bucket: Terms.Bucket =>
        val key = bucket.getKey
        val docCount = bucket.getDocCount

        println(s"$key\t$docCount")
      }
    }
  }

  val categoryOpt = args.lift(0)
  val metaDirOpt = args.lift(1)
  val rawDirOpt = args.lift(2)

  if (categoryOpt.isEmpty)
    listCategories()
  else if (metaDirOpt.isDefined && rawDirOpt.isDefined) {
    val metaDir = metaDirOpt.get
    val rawDir = rawDirOpt.get

    Files.createDirectories(Paths.get(metaDir))
    Files.createDirectories(Paths.get(rawDir))
    downloadCategory(categoryOpt.get, metaDir, rawDir)
  }
  else
    println("argumets: category | (metaDir rawDir)")
}
