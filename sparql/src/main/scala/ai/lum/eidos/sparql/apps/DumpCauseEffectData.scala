package ai.lum.eidos.sparql.apps

import java.io.File

import ai.lum.eidos.sparql.data.Dataset
import ai.lum.eidos.sparql.utils.Closer.AutoCloser
import ai.lum.eidos.sparql.utils.Counter
import ai.lum.eidos.sparql.utils.Sinker
import ai.lum.eidos.sparql.utils.StringUtils
import ai.lum.eidos.sparql.utils.TsvUtils.TsvWriter
import org.apache.jena.rdfconnection.RDFConnection
import org.apache.jena.rdfconnection.RDFConnectionFuseki

import scala.collection.mutable

object DumpCauseEffectData extends App {
  val host = "http://localhost:3030"

  def mkConnection(datasetName: String): RDFConnection = {
    val endpoint = s"$host/$datasetName/query"
    val builder = RDFConnectionFuseki
        .create()
        .destination(endpoint)

    builder.build
  }

  object Topic extends Enumeration {
    type Topic = Value
    val Sentence, Cause, Effect = Value
  }

  def mkFile(ontologyName: String, subtype: String): File = {
    new File(s"../sparql/texts/$subtype/" + StringUtils.afterFirst(ontologyName, ':') + ".tsv")
  }

  case class CountAndTsvWriter(counter: Counter, tsvWriter: TsvWriter)

  def mkTsvWriter(ontologyName: String, subtype: String, isSentence: Boolean = false): TsvWriter = {
    val file = mkFile(ontologyName, subtype)
    val printWriter = Sinker.printWriterFromFile(file)
    val tsvWriter = new TsvWriter(printWriter)

    if (isSentence)
      tsvWriter.println("sentenceSource", "sentenceType", "sentenceTrigger", "sentenceConfidence")
    else
      tsvWriter.println("subjectSource", "subjectConfidence",
        "causeType", "causeTrigger", "causeConfidence",
        "effectType", "effectTrigger", "effectConfidence"
      )
    tsvWriter
  }

  class Row(val ontologyName: String) {
    val counters: Map[Topic.Value, Counter] = Map(
      Topic.Sentence -> Counter(),
      Topic.Cause -> Counter(),
      Topic.Effect -> Counter()
    )
    val tsvWriters: Map[Topic.Value, TsvWriter] = Map(
      Topic.Sentence -> mkTsvWriter(ontologyName, "sentence", isSentence = true),
      Topic.Cause -> mkTsvWriter(ontologyName, "cause"),
      Topic.Effect-> mkTsvWriter(ontologyName, "effect")
    )

    def closeAll(): Unit = {
      tsvWriters.values.foreach(_.close())
    }
  }

  object Row {

    def getOrNew(rowMap: mutable.Map[String, Row], ontologyName: String): Row = {
      val lowerOntologyName = ontologyName.toLowerCase

      rowMap.getOrElseUpdate(lowerOntologyName, new Row(ontologyName))
    }
  }


  def mkCauseEffectQuery(datasetName: String): String =
    // Be careful: fhe stripMargin only works correctly if the variables being
    // substituted in do not contains characters that look like margins themselves.
    s"""
      |PREFIX causal: <http://ontology.causeex.com/ontology/odps/CauseEffect#>
      |PREFIX     dp: <http://ontology.causeex.com/ontology/odps/DataProvenance#>
      |PREFIX  event: <http://ontology.causeex.com/ontology/odps/Event#>
      |PREFIX     gc: <http://ontology.causeex.com/ontology/odps/GeneralConcepts#>
      |
      |PREFIX   rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |
      |SELECT ?subjectSource ?subjectConfidence ?causeType ?causeTrigger ?causeConfidence ?effectType ?effectTrigger ?effectConfidence
      |FROM <$host/$datasetName/data/$datasetName>
      |WHERE {
      |    ?subject a causal:CausalAssertion;
      |        dp:sourced_from ?subjectSource;
      |        gc:numeric_confidence ?subjectConfidence;
      |        causal:has_cause ?cause;
      |        causal:has_effect ?effect.
      |    ?cause a ?causeType;
      |        gc:description ?causeTrigger;
      |        gc:numeric_confidence ?causeConfidence.
      |    ?effect a ?effectType;
      |        gc:description ?effectTrigger;
      |        gc:numeric_confidence ?effectConfidence.
      |}
      |
      |#LIMIT 10
      |""".stripMargin

  def runCauseEffect(rowMap: mutable.Map[String, Row], connection: RDFConnection, datasetName: String): Unit = {
    val query = mkCauseEffectQuery(datasetName)

    connection.queryResultSet(query, { resultSet =>
      while (resultSet.hasNext) {
        val querySolution = resultSet.next
        val subjectSource = querySolution.getResource("subjectSource").getLocalName
        val subjectConfidence = querySolution.getLiteral("subjectConfidence").getDouble

        val causeType = querySolution.getResource("causeType").getLocalName
        val causeNamespace = querySolution.getResource("causeType").getNameSpace
        if (causeNamespace != "http://ontology.causeex.com/ontology/odps/Event#")
          println(causeNamespace)
        val causeTrigger = querySolution.getLiteral("causeTrigger").getString
        val causeConfidence = querySolution.getLiteral("causeConfidence").getDouble

        val effectType = querySolution.getResource("effectType").getLocalName
        val effectNamespace = querySolution.getResource("effectType").getNameSpace
        if (effectNamespace != "http://ontology.causeex.com/ontology/odps/Event#")
          println(effectNamespace)
        val effectTrigger = querySolution.getLiteral("effectTrigger").getString
        val effectConfidence = querySolution.getLiteral("effectConfidence").getDouble

        Array((Topic.Cause, causeType), (Topic.Effect, effectType)).foreach { case (topic: Topic.Value, subtype: String) =>
          val row = Row.getOrNew(rowMap, subtype)

          row.counters(topic).inc
          row.tsvWriters(topic).println(subjectSource, subjectConfidence.toString,
            causeType, causeTrigger, causeConfidence.toString,
            effectType, effectTrigger, effectConfidence.toString
          )
        }
      }
    })
  }

  def mkSentenceQuery(datasetName: String): String =
  // Be careful: fhe stripMargin only works correctly if the variables being
  // substituted in do not contains characters that look like margins themselves.
    s"""
      |PREFIX causal: <http://ontology.causeex.com/ontology/odps/CauseEffect#>
      |PREFIX     dp: <http://ontology.causeex.com/ontology/odps/DataProvenance#>
      |PREFIX  event: <http://ontology.causeex.com/ontology/odps/Event#>
      |PREFIX     gc: <http://ontology.causeex.com/ontology/odps/GeneralConcepts#>
      |
      |PREFIX   rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |
      |SELECT ?subjectSource ?sentenceType ?sentenceTrigger ?sentenceConfidence
      |FROM <http://localhost:3030/large01/data/large01>
      |WHERE {
      |    ?subject a causal:CausalAssertion;
      |        dp:sourced_from ?subjectSource;
      |        causal:has_cause ?cause;
      |        causal:has_effect ?effect.
      |    ?sentence dp:sourced_from ?subjectSource;
      |        a ?sentenceType;
      |        gc:numeric_confidence ?sentenceConfidence.
      |    ?subjectSource dp:text_value ?sentenceTrigger.
      |
      |    FILTER(?sentence != ?cause && ?sentence != ?effect && ?sentenceType != causal:CausalAssertion).
      |}
      |
      |#LIMIT 10
      |""".stripMargin

  def runSentence(rowMap: mutable.Map[String, Row], connection: RDFConnection, datasetName: String): Unit = {
    val query = mkSentenceQuery(datasetName)

    connection.queryResultSet(query, { resultSet =>
      while (resultSet.hasNext) {
        val querySolution = resultSet.next
        val subjectSource = querySolution.getResource("subjectSource").getLocalName
        val sentenceType = querySolution.getResource("sentenceType").getLocalName
        val namespace = querySolution.getResource("sentenceType").getNameSpace
        if (namespace != "http://ontology.causeex.com/ontology/odps/Event#")
          println(namespace)
        val sentenceTrigger = querySolution.getLiteral("sentenceTrigger").getString
        val sentenceConfidence = querySolution.getLiteral("sentenceConfidence").getDouble

        val sentenceRow = Row.getOrNew(rowMap, sentenceType)
        sentenceRow.counters(Topic.Sentence).inc
        sentenceRow.tsvWriters(Topic.Sentence).println(subjectSource,
          sentenceType, sentenceTrigger, sentenceConfidence.toString,
        )
      }
    })
  }

  def run(): Unit = {
    new TsvWriter(Sinker.printWriterFromFile("../sparql/texts/counts.tsv")).autoClose { countWriter =>
      val rowMap: mutable.Map[String, Row] = mutable.Map.empty

      countWriter.println("event", "sentence", "cause", "effect")
      Dataset.names.foreach { datasetName =>
        mkConnection(datasetName).autoClose { connection =>
          runCauseEffect(rowMap, connection, datasetName)
          runSentence(rowMap, connection, datasetName)
        }
      }
      rowMap.foreach { case (ontologyName: String, row: Row) =>
        row.closeAll()
        countWriter.println(
          row.ontologyName,
          row.counters(Topic.Sentence).get.toString,
          row.counters(Topic.Cause).get.toString,
          row.counters(Topic.Effect).get.toString
        )
      }
    }
  }

  run()
}
