package org.clulab.wm.eidos.apps

import java.io.ByteArrayOutputStream
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
import org.apache.http.HttpHost
import org.apache.http.HttpRequestInterceptor
import org.clulab.utils.Closer.AutoCloser
import org.elasticsearch.action.search.SearchRequest
import org.elasticsearch.client.Request
import org.elasticsearch.client.Response
import org.elasticsearch.client.RestClient
import org.elasticsearch.client.RestHighLevelClient
import org.elasticsearch.client.RequestOptions
import org.elasticsearch.index.query.QueryBuilders
import org.elasticsearch.script.ScriptType
import org.elasticsearch.script.mustache.SearchTemplateRequest
import org.elasticsearch.search.builder.SearchSourceBuilder

object ElasticSearch extends App {
  def test1(): Unit = {
    val profileName = "elasticsearch"
    val awsCredentialsProvider: AWSCredentialsProvider = new ProfileCredentialsProvider(profileName)
    val serviceEndpoint = "search-world-modelers-dev-gjvcliqvo44h4dgby7tn3psw74.us-east-1.es.amazonaws.com"
    val region = "us-east-1"
    val endpointConfiguration = new EndpointConfiguration(serviceEndpoint, region)
    val elasticsearchClientBuilder = AWSElasticsearchClientBuilder.standard()
        .withEndpointConfiguration(endpointConfiguration)
        .withCredentials(awsCredentialsProvider)
    val awsElasticsearch = elasticsearchClientBuilder.build
    val awsElasticsearchClient = awsElasticsearch.asInstanceOf[AWSElasticsearchClient]
    val listTagsResult = awsElasticsearchClient.listTags(new ListTagsRequest())
  }

  def test2(): Unit = {
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

    def runSearchSource(): Unit = {
      newRestHighLevelClient().autoClose { restHighLevelClient =>
        // For this see particularly https://www.elastic.co/guide/en/elasticsearch/client/java-rest/7.2/_search_apis.html
        val indexName = "wm-dev"

        val searchSourceBuilder = new SearchSourceBuilder()
        searchSourceBuilder.query(QueryBuilders.matchAllQuery())
        val searchRequest = new SearchRequest(indexName)
        searchRequest.source(searchSourceBuilder)

        val requestOptions = RequestOptions.DEFAULT
        val searchResponse = restHighLevelClient.search(searchRequest, requestOptions)
        val status = searchResponse.status
        val hits = searchResponse.getHits()

        println(hits)
      }
    }

    def runSearchTemplate(): Unit = {
      newRestHighLevelClient().autoClose { restHighLevelClient =>
        // For this see particularly https://www.elastic.co/guide/en/elasticsearch/client/java-rest/7.2/_search_apis.html
        val indexName = "wm-dev"

        val searchTemplateRequest = new SearchTemplateRequest()
        searchTemplateRequest.setRequest(new SearchRequest(indexName))
        searchTemplateRequest.setScriptType(ScriptType.INLINE)
        searchTemplateRequest.setScript(
          """{
            |    "aggs" : {
            |        "categories" : {
            |            "terms" : { "field" : "category.keyword" }
            |        }
            |    }
            |}
          """.stripMargin)
        searchTemplateRequest.setScriptParams(new util.HashMap[String, AnyRef])

        val requestOptions = RequestOptions.DEFAULT
        val searchTemplateResponse = restHighLevelClient.searchTemplate(searchTemplateRequest, requestOptions)
        val status = searchTemplateResponse.status
        val searchResponse = searchTemplateResponse.getResponse

        val hits = searchResponse.getHits()
        println(hits)
      }
    }

    def runS3(): Unit = {
      val amazonS3 = newS3Client()
//      val buckets = amazonS3.listBuckets()
      val bucketName = "world-modelers"
//      val key = "migration/tmp/DEV/MONTHLY20PRICE20WATCHANDANNEX08312015.pdf"
      val key = "documents/migration/South_Sudan_army,_rebels_trade_control_over_Pagak_town_Aug-17.html"
      val s3Object = amazonS3.getObject(bucketName, key)
      val s3ObjectInputStream = s3Object.getObjectContent()

      val contents = new ByteArrayOutputStream().autoClose { byteArrayOutputStream =>
        val readBuf = new Array[Byte](1024)
        var readLen = 0

        def read(): Boolean = {
          readLen =  s3ObjectInputStream.read(readBuf)
          readLen > 0
        }

        while (read())
          byteArrayOutputStream.write(readBuf, 0, readLen)

        new String(byteArrayOutputStream.toByteArray)
      }

      println(contents) // Would want to put this into file, the bytearray already
//      println(buckets)
    }

//    runSearchSource()
//    runSearchTemplate()
    runS3()
  }

  test2()
}
