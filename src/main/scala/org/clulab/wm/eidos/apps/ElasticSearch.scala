package org.clulab.wm.eidos.apps

import java.io.ByteArrayOutputStream
import java.io.File
import java.net.URL
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util

import com.amazonaws.auth.AWS4Signer
import com.amazonaws.auth.AWSCredentials
import com.amazonaws.auth.AWSCredentialsProvider
import com.amazonaws.auth.profile.ProfileCredentialsProvider
import com.amazonaws.client.builder.AwsClientBuilder.EndpointConfiguration
import com.amazonaws.http.AWSRequestSigningApacheInterceptor
import com.amazonaws.services.elasticsearch.AWSElasticsearchClient
import com.amazonaws.services.elasticsearch.AWSElasticsearchClientBuilder
import com.amazonaws.services.elasticsearch.model.ListTagsRequest
import com.amazonaws.services.s3.AmazonS3
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.AmazonS3ClientBuilder
import com.amazonaws.services.s3.model.GetObjectRequest
import com.amazonaws.services.s3.model.S3ObjectInputStream
import org.apache.http.HttpHost
import org.apache.http.HttpRequestInterceptor
import org.clulab.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.StringUtils
import org.elasticsearch.action.search.SearchRequest
import org.elasticsearch.action.search.SearchResponse
import org.elasticsearch.client.Request
import org.elasticsearch.client.Response
import org.elasticsearch.client.RestClient
import org.elasticsearch.client.RestHighLevelClient
import org.elasticsearch.client.RequestOptions
import org.elasticsearch.index.query.QueryBuilders
import org.elasticsearch.rest.RestStatus
import org.elasticsearch.script.ScriptType
import org.elasticsearch.script.mustache.SearchTemplateRequest
import org.elasticsearch.search.SearchHit
import org.elasticsearch.search.aggregations.Aggregation
import org.elasticsearch.search.aggregations.Aggregations
import org.elasticsearch.search.aggregations.bucket.terms.ParsedStringTerms
import org.elasticsearch.search.aggregations.bucket.terms.StringTerms
import org.elasticsearch.search.aggregations.bucket.terms.Terms
import org.elasticsearch.search.builder.SearchSourceBuilder
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods

import scala.collection.mutable



object ElasticSearch extends App {
  val indexName = "wm-dev"

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
    val interceptor: HttpRequestInterceptor = new AWSRequestSigningApacheInterceptor(serviceName, aws4signer, awsCredentialsProvider);
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

    searchTemplateRequest.setRequest(new SearchRequest(indexName))
    searchTemplateRequest.setScriptType(ScriptType.INLINE)
    searchTemplateRequest.setScript(script)
    searchTemplateRequest.setScriptParams(new util.HashMap[String, AnyRef])
    searchTemplateRequest
  }

  protected def writeMeta(text: String, metaDir: String, id: String, fileType: String): Unit = {
    val filename = metaDir + File.separatorChar + id + fileType

    FileUtils.printWriterFromFile(filename).autoClose { printWriter =>
      printWriter.println(text)
    }
  }

  protected def writeRaw(s3Client: AmazonS3, storedUrl: String, rawDir: String, id: String, fileType: String): Unit = {
    val filename = rawDir + File.separatorChar + id + fileType
    val bucketName = "world-modelers"
    val key = StringUtils.afterFirst(new URL(storedUrl).getFile, '/')
    val s3Object = s3Client.getObject(bucketName, key)
    val getObjectRequest = new GetObjectRequest(bucketName, key)

    s3Client.getObject(getObjectRequest, new File(filename))
  }

  def downloadCategory(category: String, metaDir: String, rawDir: String): Unit = {
    newRestHighLevelClient().autoClose { restHighLevelClient =>
      // TODO: This assumes that $category does not need to be escaped to be valid JSON!
      val script =
        s"""
          |{
          |  "query" : {
          |    "match" : {
          |      "category" : "$category"
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

      val hits = searchTemplateResponse.getResponse.getHits

      if (hits.totalHits > 0) {
        val s3Client = newS3Client()

        hits.forEach { hit: SearchHit =>
          implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

          val id = hit.getId
          val text = hit.toString
          val json = hit.getSourceAsString()
          val jValue = JsonMethods.parse(json)
          val fileType = (jValue \ "file_type").extract[String]
          val storedUrl = (jValue \ "stored_url").extract[String]

          writeMeta(text, metaDir, id, ".json")
          writeRaw(s3Client, storedUrl, rawDir, id, fileType)
        }
      }
    }
  }

  def listCategories(): Unit = {
    newRestHighLevelClient().autoClose { restHighLevelClient =>
      // For this see particularly https://www.elastic.co/guide/en/elasticsearch/client/java-rest/7.2/_search_apis.html
      val categories = "categories"
      // TODO: This probably needs some kind of scroller for all results
      val script =
        s"""
          |{
          |  "aggs" : {
          |    "${categories}" : {
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

      val searchResponse: SearchResponse = searchTemplateResponse.getResponse()
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
    println("argumets: [category metaDir rawDir]")
}
