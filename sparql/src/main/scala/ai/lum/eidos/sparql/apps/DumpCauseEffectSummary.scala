package ai.lum.eidos.sparql.apps

import ai.lum.eidos.sparql.data.Dataset
import ai.lum.eidos.sparql.utils.Closer.AutoCloser
import ai.lum.eidos.sparql.utils.Counter
import ai.lum.eidos.sparql.utils.FileUtils
import ai.lum.eidos.sparql.utils.Sinker
import ai.lum.eidos.sparql.utils.StringUtils
import ai.lum.eidos.sparql.utils.TsvUtils.TsvWriter
import org.apache.jena.rdfconnection.RDFConnection
import org.apache.jena.rdfconnection.RDFConnectionFuseki

object DumpCauseEffectSummary extends App {
  val host = "http://localhost:3030"

  def mkConnection(datasetName: String): RDFConnection = {
    val endpoint = s"$host/$datasetName/query"
    val builder = RDFConnectionFuseki
        .create()
        .destination(endpoint)

    builder.build
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
      |SELECT ?canonicalLabel
      |FROM <$host/$datasetName/data/$datasetName>
      |WHERE {
      |    ?subject a causal:CausalAssertion;
      |        dp:sourced_from ?subjectSource;
      |        causal:has_cause ?cause;
      |        causal:has_effect ?effect.
      |    ?container dp:contains ?subjectSource;
      |        gc:canonical_label ?canonicalLabel.
      |}
      |
      |#LIMIT 10
      |""".stripMargin

  def runCauseEffect(counters: Map[String, Counter], connection: RDFConnection, datasetName: String): Unit = {
    val query = mkCauseEffectQuery(datasetName)

    connection.queryResultSet(query, { resultSet =>
      while (resultSet.hasNext) {
        val querySolution = resultSet.next
        val canonicalLabel = querySolution.getLiteral("canonicalLabel").getString

        counters.get(canonicalLabel).foreach { counter =>
          counter.inc()
        }
      }
    })
  }

  def run(): Unit = {
    val inputDir = args(0)
    val outputFile = args(1)

    val counters: Map[String, Counter] = {
      val files = FileUtils.findFiles(inputDir, "jsonld")

      files.map { file =>
        StringUtils.beforeLast(file.getName, '.') -> Counter()
      }.toMap
    }

    val expectedType = "relation"
    val expectedSubtype = "causation"

    new TsvWriter(Sinker.printWriterFromFile(outputFile)).autoClose { tsvWriter =>
      tsvWriter.println("docId", "type", "subtype", "lccCount")
      Dataset.names.foreach { datasetName =>
        mkConnection(datasetName).autoClose { connection =>
          runCauseEffect(counters, connection, datasetName)
        }
      }

      val total = Counter()

      counters.keys.toSeq.sorted.foreach { key =>
        val count = counters(key).get

        tsvWriter.println(StringUtils.beforeLast(key, '.'), expectedType, expectedSubtype, count.toString)
        total.inc(count)
      }
      tsvWriter.println()
      tsvWriter.println("total", expectedType, expectedSubtype, total.get.toString)
    }
  }

  run()
}
