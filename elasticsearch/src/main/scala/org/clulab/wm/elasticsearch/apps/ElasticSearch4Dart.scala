package org.clulab.wm.elasticsearch.apps

import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import java.time.ZonedDateTime
import java.util

import org.apache.http.HttpHost
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.Sinker
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
    // IntelliJ would rather have Any instead of AnyRef, but that's wrong.
    searchTemplateRequest.setScriptParams(new util.HashMap[String, AnyRef])
    searchTemplateRequest
  }

  protected def writeFile(text: String, fileDir: String, id: String, fileType: String): Unit = {
    val filename = fileDir + File.separatorChar + id + fileType

    Sinker.printWriterFromFile(filename).autoClose { printWriter =>
      printWriter.println(text)
    }
  }

  protected def processHit(metaDir: String, textDir: String)(hit: SearchHit): Unit = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    val id = hit.getId
    val metaText = hit.toString
    val json = hit.getSourceAsString
    val jValue = JsonMethods.parse(json)
    val extractedText = try {
      (jValue \ "extracted_text").extract[String]
    }
    catch {
      case throwable: Throwable =>
        println(s"For $id the following exception is noted:")
        throwable.printStackTrace()
        ""
    }

    writeFile(metaText, metaDir, id, ".json")
    writeFile(extractedText, textDir, id, ".txt")
  }

  protected def processScript(script: String, function: SearchHit => Unit): Unit = {
    newRestHighLevelClient().autoClose { restHighLevelClient =>
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
          hits.forEach(hit => function(hit))

          continue = scrollIdOpt.exists { scrollId =>
            // See https://www.elastic.co/guide/en/elasticsearch/client/java-rest/master/java-rest-high-search-scroll.html
            val searchScrollRequest = new SearchScrollRequest(scrollId)

            searchScrollRequest.scroll(TimeValue.timeValueSeconds(timeout)) // It is important to rescroll the scroll!

            val searchResponse: SearchResponse = restHighLevelClient.scroll(searchScrollRequest, RequestOptions.DEFAULT)

            scrollIdOpt = Option(searchResponse.getScrollId)
            hits = searchResponse.getHits
            hits.getHits.length > 0
          }
        } while (continue)

        scrollIdOpt.foreach { scrollId =>
          val clearScrollRequest = new ClearScrollRequest()

          clearScrollRequest.addScrollId(scrollId)
          restHighLevelClient.clearScroll(clearScrollRequest, RequestOptions.DEFAULT)
        }
      }
    }
  }

  def downloadTextSearch(searchOpt: Option[String], metaDir: String, textDir: String): Unit = {
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
      """ }.getOrElse {
      s"""
        |{
        |  "size" : $resultsPerQuery,
        |  "query" : {
        |    "match_all" : {}
        |  }
        |}
      """
    }.stripMargin

    processScript(script, processHit(metaDir, textDir))
  }

  def downloadCutoffSearch(cutoff: String, metaDir: String, textDir: String): Unit = {
    val jsonCutoff = JsonMethods.pretty(new JString(cutoff))
    val script =
      s"""
        |{
        |  "size" : $resultsPerQuery,
        |  "query" : {
        |    "range" : {
        |      "timestamp" : {
        |        "gt" : $jsonCutoff
        |      }
        |    }
        |  }
        |}
      """.stripMargin

    processScript(script, processHit(metaDir, textDir))
  }

  def downloadCategory(category: String, metaDir: String, textDir: String): Unit = {
    val jsonCategory = JsonMethods.pretty(new JString(category))
    // Use "term" for exact matches, "match" for fuzzy matches.
    val script =
      s"""
        |{
        |  "size" : $resultsPerQuery,
        |  "query" : {
        |    "term" : {
        |      "categories.keyword" : {
        |        "value" : $jsonCategory
        |      }
        |    }
        |  }
        |}
      """.stripMargin

    processScript(script, processHit(metaDir, textDir))
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
          |      "terms" : { "field" : "categories.keyword" }
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

  val commandOpt = args.lift(0)
  val metaDirOpt = args.lift(1)
  val textDirOpt = args.lift(2)

  val categoryOpt = args.lift(3)
  val searchOpt = args.lift(3)
  val cutoffOpt = args.lift(3)

  println(s"It is now ${ZonedDateTime.now()} locally.")
  if (commandOpt.isDefined && commandOpt.get == "time" && metaDirOpt.isDefined && textDirOpt.isDefined && cutoffOpt.isDefined) {
    val metaDir = metaDirOpt.get
    val textDir = textDirOpt.get
    val cutoff = cutoffOpt.get

    Files.createDirectories(Paths.get(metaDir))
    Files.createDirectories(Paths.get(textDir))
    downloadCutoffSearch(cutoff, metaDir, textDir)
  }
  else if (commandOpt.isDefined && commandOpt.get == "text" && metaDirOpt.isDefined && textDirOpt.isDefined) {
    val metaDir = metaDirOpt.get
    val textDir = textDirOpt.get

    Files.createDirectories(Paths.get(metaDir))
    Files.createDirectories(Paths.get(textDir))
    downloadTextSearch(searchOpt, metaDir, textDir)
  }
  else if (commandOpt.isDefined && commandOpt.get == "cat" && metaDirOpt.isDefined && textDirOpt.isDefined && categoryOpt.isDefined) {
    val metaDir = metaDirOpt.get
    val textDir = textDirOpt.get

    Files.createDirectories(Paths.get(metaDir))
    Files.createDirectories(Paths.get(textDir))
    downloadCategory(categoryOpt.get, metaDir, textDir)
  }
  else if (commandOpt.isDefined && commandOpt.get == "cats") {
    listCategories()
  }
  else {
    println("arguments: time <metaDir> <textDir> <cutoffDateTime> (for downloads after cutoffDate)")
    println("arguments: text <metaDir> <textDir> [<searchText>] (for downloads based on searchText)")
    println("arguments: cat <metaDir> <textDir> <category> (for downloads based on category)")
    println("arguments: cats (for list of categories)")
  }
}
