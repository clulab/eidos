package ai.lum.eidos.sparql.apps

import java.io.File
import java.io.PrintWriter

import ai.lum.eidos.sparql.data.Dataset
import ai.lum.eidos.sparql.data.Ontology
import ai.lum.eidos.sparql.utils.Closer.AutoCloser
import ai.lum.eidos.sparql.utils.Sinker
import ai.lum.eidos.sparql.utils.StringUtils
import org.apache.jena.rdfconnection.RDFConnection
import org.apache.jena.rdfconnection.RDFConnectionFuseki

object DumpOntology extends App {
  val host = "http://localhost:3030"

  def mkConnection(datasetName: String): RDFConnection = {
    val endpoint = s"$host/$datasetName/query"
    val builder = RDFConnectionFuseki
        .create()
        .destination(endpoint)

    builder.build
  }

  def mkQuery(datasetName: String, ontologyName: String): String =
    s"""
      |PREFIX data-prov: <http://ontology.causeex.com/ontology/odps/DataProvenance#>
      |PREFIX event: <http://ontology.causeex.com/ontology/odps/Event#>
      |PREFIX gc: <http://ontology.causeex.com/ontology/odps/GeneralConcepts#>
      |PREFIX icm: <http://ontology.causeex.com/ontology/odps/ICM#>
      |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |PREFIX lcc: <http://www.languagecomputer.com/lcc#>
      |PREFIX src: <http://graph.causeex.com/documents/sources#>
      |
      |SELECT ?text
      |FROM <$host/$datasetName/data/$datasetName>
      |WHERE {
      |    ?doc a data-prov:Document;                         # Find a document.
      |        data-prov:contains* ?component.                # It must have some components.
      |    ?event data-prov:sourced_from* ?component;         # Some events are sourced_from these components.
      |        a ?eventType.                                  # They are of a certain event type.
      |    ?eventType rdfs:subClassOf* event:Event.           # That event type is a subClassOf Event.
      |    ?event icm:has_factor ?factor.                     # The event has_factor factor
      |    ?factor icm:has_factor_type $ontologyName.         # And that factor has_factor_type of a certain value
      |    ?component data-prov:text_value ?text.             # In that case, get the text of the component.
      |}
      |
      |LIMIT 100
      |""".stripMargin

  def mkFile(ontologyName: String): File = {
    new File("./texts/" + StringUtils.afterFirst(ontologyName, ':') + ".txt")
  }

  // Make sure the text stays on one line and can be used in tsv file.
  def escape(text: String): String = text
      .replace("\\", "\\\\")
      .replace("\t", "\\t")
      .replace("\n", "\\n")
      .replace("\r", "\\r")

  def run(countPrintWriter: PrintWriter, ontologyName: String): Unit = {
    var count = 0

    Sinker.printWriterFromFile(mkFile(ontologyName)).autoClose { printWriter =>
      Dataset.names.foreach { datasetName =>
        // There seem to be multiple events of the same kind in the same sentence.
        // The query delivers them in order, so this is essentially implements DISTINCT.
        var prevText = ""

        mkConnection(datasetName).autoClose { connection =>
          val query = mkQuery(datasetName, ontologyName)

          connection.queryResultSet(query, { resultSet =>
            while (resultSet.hasNext()) {
              val querySolution = resultSet.next
              val text = querySolution.getLiteral("text").getString

              if (text != prevText) {
                printWriter.println(escape(text))
                count += 1
              }
              prevText = text
            }
          })
        }
      }
    }
    countPrintWriter.println(s"$ontologyName\t$count")
  }

  Sinker.printWriterFromFile("counts.txt").autoClose { printWriter =>
    Ontology.names.foreach { ontologyName =>
      run(printWriter, ontologyName)
    }
  }
}
