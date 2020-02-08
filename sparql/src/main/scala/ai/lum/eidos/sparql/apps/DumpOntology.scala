package ai.lum.eidos.sparql.apps

import java.io.File

import ai.lum.eidos.sparql.data.Dataset
import ai.lum.eidos.sparql.data.Ontology
import ai.lum.eidos.sparql.utils.Closer.AutoCloser
import ai.lum.eidos.sparql.utils.Sinker
import ai.lum.eidos.sparql.utils.StringUtils
import org.apache.jena.query.QuerySolution
import org.apache.jena.query.ResultSetFormatter
import org.apache.jena.rdfconnection.RDFConnection
import org.apache.jena.rdfconnection.RDFConnectionFuseki

object DumpOntology extends App {

  def mkConnection(dataset: String): RDFConnection = {
    val host = s"http://localhost:3030/$dataset/query"
    val builder = RDFConnectionFuseki
        .create()
        .destination(host)

    builder.build
  }

  def mkQuery(dataset: String): String =
    s"""
      |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |
      |SELECT ?subject ?predicate ?object
      |FROM <http://localhost:3030/$dataset/data/$dataset>
      |WHERE {
      |  ?subject ?predicate ?object
      |}
      |LIMIT 25
      |""".stripMargin

  def mkFile(ontologyName: String): File = {
    new File(StringUtils.afterFirst(ontologyName, ':') + ".txt")
  }

  // Make sure the text stays on one line and can be used in tsv file.
  def escape(text: String): String = text
      .replace("\\", "\\\\")
      .replace("\t", "\\t")
      .replace("\n", "\\n")
      .replace("\r", "\\r")

  def run(ontologyName: String): Unit = {
    var count = 0

    Sinker.newBufferedOutputStream(mkFile(ontologyName)).autoClose { outputStream =>
      Dataset.names.foreach { datasetName =>
        val connection = mkConnection(datasetName)
        val query = mkQuery(datasetName)

        connection.queryResultSet(query, { resultSet =>
          ResultSetFormatter.outputAsTSV(outputStream, resultSet)
//          val text = ResultSetFormatter.asText(subject)
//          val text = subject.toString
          count += 1
//          printWriter.println(escape(text))
        })
      }
    }
    println(s"For ontology $ontologyName there were $count entries.")
  }

  Ontology.names.foreach { ontologyName =>
    run(ontologyName)
  }
}
