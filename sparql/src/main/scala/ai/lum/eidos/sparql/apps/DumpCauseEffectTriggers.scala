package ai.lum.eidos.sparql.apps

import ai.lum.eidos.sparql.data.Dataset
import ai.lum.eidos.sparql.utils.Closer.AutoCloser
import ai.lum.eidos.sparql.utils.Counter
import ai.lum.eidos.sparql.utils.Sinker
import ai.lum.eidos.sparql.utils.TsvUtils.TsvWriter
import org.apache.jena.rdfconnection.RDFConnection
import org.apache.jena.rdfconnection.RDFConnectionFuseki

import scala.collection.mutable

object DumpCauseEffectTriggers extends App {
  val host = "http://localhost:3030"

  val countFilename = args(0)

  def mkConnection(datasetName: String): RDFConnection = {
    val endpoint = s"$host/$datasetName/query"
    val builder = RDFConnectionFuseki
        .create()
        .destination(endpoint)

    builder.build
  }

  class Row(val ontologyName: String) {
    // These Counters are cause, effect, and total.
    val counterMap: mutable.Map[String, (Counter, Counter, Counter)] = mutable.Map.empty

    def incCause(trigger: String): Unit = {
      val value = counterMap.getOrElseUpdate(trigger, (Counter(), Counter(), Counter()))

      value._1.inc()
      value._3.inc()
    }

    def incEffect(trigger: String): Unit = {
      val value = counterMap.getOrElseUpdate(trigger, (Counter(), Counter(), Counter()))

      value._2.inc()
      value._3.inc()
    }

    def getCause(trigger: String): Int = counterMap(trigger)._1.get

    def getEffect(trigger: String): Int = counterMap(trigger)._2.get

    def getTotal(trigger: String): Int = counterMap(trigger)._3.get
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
      |PREFIX     gc: <http://ontology.causeex.com/ontology/odps/GeneralConcepts#>
      |
      |PREFIX   rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |
      |SELECT ?causeType ?causeTrigger ?effectType ?effectTrigger
      |FROM <$host/$datasetName/data/$datasetName>
      |WHERE {
      |    ?subject a causal:CausalAssertion;
      |        dp:sourced_from ?subjectSource;
      |        causal:has_cause ?cause;
      |        causal:has_effect ?effect.
      |    ?cause a ?causeType;
      |        gc:description ?causeTrigger.
      |    ?effect a ?effectType;
      |        gc:description ?effectTrigger.
      |}
      |
      |#LIMIT 10
      |""".stripMargin

  def extractText(trigger: String): String = {
    val left = trigger.indexOf("[[")
    val right = trigger.lastIndexOf("]]")
    val text = trigger.substring(left + 2, right)

    text
  }

  def runCauseEffect(ontologyNameMap: mutable.Map[String, Row], connection: RDFConnection, datasetName: String): Unit = {
    val query = mkCauseEffectQuery(datasetName)

    connection.queryResultSet(query, { resultSet =>
      while (resultSet.hasNext) {
        val querySolution = resultSet.next

        val causeType = querySolution.getResource("causeType").getLocalName
        val causeNamespace = querySolution.getResource("causeType").getNameSpace
        if (causeNamespace != "http://ontology.causeex.com/ontology/odps/Event#")
          println(causeNamespace)
        val causeTrigger = querySolution.getLiteral("causeTrigger").getString
        val causeText = extractText(causeTrigger)

        val effectType = querySolution.getResource("effectType").getLocalName
        val effectNamespace = querySolution.getResource("effectType").getNameSpace
        if (effectNamespace != "http://ontology.causeex.com/ontology/odps/Event#")
          println(effectNamespace)
        val effectTrigger = querySolution.getLiteral("effectTrigger").getString
        val effectText = extractText(effectTrigger)

        val causeRow = Row.getOrNew(ontologyNameMap, causeType)
        causeRow.incCause(causeText)

        val effectRow = Row.getOrNew(ontologyNameMap, effectType)
        effectRow.incEffect(effectText)
      }
    })
  }

  def run(countFilename: String): Unit = {
    new TsvWriter(Sinker.printWriterFromFile(countFilename)).autoClose { countWriter =>
      val perOntologyName: mutable.Map[String, Row] = mutable.Map.empty

      countWriter.println("EventType", "Participant (Cause/Effect)", "Count", "Mention")
      Dataset.names.foreach { datasetName =>
        mkConnection(datasetName).autoClose { connection =>
          runCauseEffect(perOntologyName, connection, datasetName)
        }
      }
      perOntologyName.keys.toSeq.sorted.foreach { ontologyName =>
        val row = perOntologyName(ontologyName)
        val rows = row.counterMap.toSeq.sortBy { case (trigger, _) =>
          (-row.getTotal(trigger), trigger)
        }
        rows.foreach { case (trigger, _) =>
          countWriter.println(row.ontologyName, "cause", row.getCause(trigger).toString, trigger)
          countWriter.println(row.ontologyName, "effect", row.getEffect(trigger).toString, trigger)
//          countWriter.println(row.ontologyName, "total", row.getTotal(trigger).toString, trigger)
        }
      }
    }
  }

  run(countFilename)
}
