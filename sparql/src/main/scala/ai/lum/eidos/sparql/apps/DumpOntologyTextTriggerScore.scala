package ai.lum.eidos.sparql.apps

import java.io.File

import ai.lum.eidos.sparql.data.Dataset
import ai.lum.eidos.sparql.data.Ontology
import ai.lum.eidos.sparql.utils.Closer.AutoCloser
import ai.lum.eidos.sparql.utils.Counter
import ai.lum.eidos.sparql.utils.ShortTermMemory
import ai.lum.eidos.sparql.utils.Sinker
import ai.lum.eidos.sparql.utils.StringUtils
import ai.lum.eidos.sparql.utils.TsvUtils
import org.apache.jena.rdfconnection.RDFConnection
import org.apache.jena.rdfconnection.RDFConnectionFuseki

object DumpOntologyTextTriggerScore extends App {
  val host = "http://localhost:3030"

  def mkConnection(datasetName: String): RDFConnection = {
    val endpoint = s"$host/$datasetName/query"
    val builder = RDFConnectionFuseki
        .create()
        .destination(endpoint)

    builder.build
  }

  def mkQuery(datasetName: String): String =
    // Be careful: fhe stripMargin only works correctly if the variables being
    // substituted in do not contains characters that look like margins themselves.
    s"""
      |PREFIX prov: <http://ontology.causeex.com/ontology/odps/DataProvenance#>
      |PREFIX event: <http://ontology.causeex.com/ontology/odps/Event#>
      |PREFIX gc: <http://ontology.causeex.com/ontology/odps/GeneralConcepts#>
      |PREFIX icm: <http://ontology.causeex.com/ontology/odps/ICM#>
      |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |PREFIX lcc: <http://www.languagecomputer.com/lcc#>
      |PREFIX src: <http://graph.causeex.com/documents/sources#>
      |
      |SELECT ?ontologyName ?relevance ?text ?description
      |FROM <$host/$datasetName/data/$datasetName>
      |WHERE {
      |    ?doc a prov:Document;                      # Find a document.
      |        prov:contains* ?component.             # It must have some components.
      |    ?event prov:sourced_from* ?component;      # Some events are sourced_from these components.
      |        a ?eventType.                          # They are of a certain event type.
      |    ?eventType rdfs:subClassOf* event:Event.   # That event type is a subClassOf Event.
      |    ?event icm:has_factor ?factor;             # The event has_factor factor
      |        gc:description ?description.           # and a description.
      |    ?factor icm:has_factor_type ?ontologyName; # And that factor has_factor_type of a certain value
      |        icm:has_relevance ?relevance.          # along with its relevance.
      |    ?component prov:text_value ?text.          # In that case, get the text of the component.
      |}
      |
      |#LIMIT 100
      |""".stripMargin

  def mkFile(ontologyName: String): File = {
    new File("../sparql/texts/" + StringUtils.afterFirst(ontologyName, ':') + ".tsv")
  }

  case class Row(ontologyName: String, relevance: Double, text: String, description: String)

  def run(ontologyNames: Array[String]): Unit = {
    val counter = Counter()

    val tsvWriters = ontologyNames.map { ontologyName =>
      val printWriter = Sinker.printWriterFromFile(mkFile(ontologyName))
      val tsvWriter = new TsvUtils.TsvWriter(printWriter)

      tsvWriter.printlnExcel("ontologyName", "relevance", "text", "description")
      StringUtils.afterFirst(ontologyName, ':') -> tsvWriter
    }.toMap
    // There seem to be multiple events of the same kind in the same sentence.
    // The query delivers them in order, so this is essentially implements DISTINCT.
    val shortTermMemories = tsvWriters.map { case (key, _) =>
      key -> ShortTermMemory[Row]
    }

    {
      Dataset.names.foreach { datasetName =>
        mkConnection(datasetName).autoClose { connection =>
          val query = mkQuery(datasetName)

          connection.queryResultSet(query, { resultSet =>
            while (resultSet.hasNext()) {
              val querySolution = resultSet.next
              val ontologyName = querySolution.getResource("ontologyName").getLocalName
              val relevance = querySolution.getLiteral("relevance").getDouble
              val text = querySolution.getLiteral("text").getString
              val description = querySolution.getLiteral("description").getString
              val row = Row(ontologyName, relevance, text, description)

              if (shortTermMemories(ontologyName).isDifferent(row)) {
                val tsvWriter = tsvWriters(ontologyName)

                tsvWriter.printlnExcel(ontologyName, relevance.toString, text, description)
                counter.inc
              }
            }
          })
        }
      }
      tsvWriters.values.foreach { tsvWriter => tsvWriter.flush }
    }
    tsvWriters.values.foreach { tsvWriter => tsvWriter.close() }
  }

  run(Ontology.names)
}
