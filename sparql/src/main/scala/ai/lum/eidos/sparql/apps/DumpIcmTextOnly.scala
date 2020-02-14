package ai.lum.eidos.sparql.apps

import java.io.File
import java.io.PrintWriter

import ai.lum.eidos.sparql.data.Dataset
import ai.lum.eidos.sparql.data.IcmOntology
import ai.lum.eidos.sparql.utils.Closer.AutoCloser
import ai.lum.eidos.sparql.utils.Counter
import ai.lum.eidos.sparql.utils.ShortTermMemory
import ai.lum.eidos.sparql.utils.Sinker
import ai.lum.eidos.sparql.utils.StringUtils
import org.apache.jena.rdfconnection.RDFConnection
import org.apache.jena.rdfconnection.RDFConnectionFuseki

object DumpIcmTextOnly extends App {
  val host = "http://localhost:3030"

  def mkConnection(datasetName: String): RDFConnection = {
    val endpoint = s"$host/$datasetName/query"
    val builder = RDFConnectionFuseki
        .create()
        .destination(endpoint)

    builder.build
  }

  def mkQuery(datasetName: String, ontologyName: String): String =
    // Be careful: fhe stripMargin only works correctly if the variables being
    // substituted in do not contains characters that look like margins themselves.
    s"""
      |PREFIX event: <http://ontology.causeex.com/ontology/odps/Event#>
      |PREFIX    gc: <http://ontology.causeex.com/ontology/odps/GeneralConcepts#>
      |PREFIX   icm: <http://ontology.causeex.com/ontology/odps/ICM#>
      |PREFIX  prov: <http://ontology.causeex.com/ontology/odps/DataProvenance#>
      |
      |PREFIX  rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |
      |SELECT ?text
      |FROM <$host/$datasetName/data/$datasetName>
      |WHERE {
      |    ?doc a prov:Document;                      # Find a document.
      |        prov:contains* ?component.             # It must have some components.
      |    ?event prov:sourced_from* ?component;      # Some events are sourced_from these components.
      |        a ?eventType.                          # They are of a certain event type.
      |    ?eventType rdfs:subClassOf* event:Event.   # That event type is a subClassOf Event.
      |    ?event icm:has_factor ?factor.             # The event has_factor factor
      |    ?factor icm:has_factor_type $ontologyName. # And that factor has_factor_type of a certain value
      |    ?component prov:text_value ?text.          # In that case, get the text of the component.
      |}
      |
      |#LIMIT 100
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
    val counter = Counter()

    Sinker.printWriterFromFile(mkFile(ontologyName)).autoClose { printWriter =>
      // There seem to be multiple events of the same kind in the same sentence.
      // The query delivers them in order, so this is essentially implements DISTINCT.
      val shortTermMemory = ShortTermMemory[String]

      Dataset.names.foreach { datasetName =>
        mkConnection(datasetName).autoClose { connection =>
          val query = mkQuery(datasetName, ontologyName)

          connection.queryResultSet(query, { resultSet =>
            while (resultSet.hasNext()) {
              val querySolution = resultSet.next
              val text = querySolution.getLiteral("text").getString

              if (shortTermMemory.isDifferent(text)) {
                printWriter.println(escape(text))
                counter.inc
              }
            }
          })
        }
      }
    }
    countPrintWriter.println(s"$ontologyName\t${counter.get}")
    countPrintWriter.flush()
  }

  Sinker.printWriterFromFile("counts.txt").autoClose { printWriter =>
    IcmOntology.names.foreach { ontologyName =>
      run(printWriter, ontologyName)
    }
  }
}
