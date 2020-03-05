package ai.lum.eidos.sparql.apps

import java.io.File
import java.io.PrintWriter

import ai.lum.eidos.sparql.data.Dataset
import ai.lum.eidos.sparql.utils.Bagger
import ai.lum.eidos.sparql.utils.Closer.AutoCloser
import ai.lum.eidos.sparql.utils.Counter
import ai.lum.eidos.sparql.utils.FileUtils
import ai.lum.eidos.sparql.utils.HashCodeBagger
import ai.lum.eidos.sparql.utils.Sourcer
import ai.lum.eidos.sparql.utils.TsvUtils
import ai.lum.eidos.sparql.utils.TsvUtils.TsvWriter
import org.apache.jena.rdfconnection.RDFConnection
import org.apache.jena.rdfconnection.RDFConnectionFuseki

object DumpActors extends App {
  val host = "http://localhost:3030"

  val inputFile = new File(args(0))
  val outputFile = new File(args(1))
  val outputDir = args(2)

  def mkConnection(datasetName: String): RDFConnection = {
    val endpoint = s"$host/$datasetName/query"
    val builder = RDFConnectionFuseki
        .create()
        .destination(endpoint)

    builder.build
  }

  def mkSentenceQuery(datasetName: String, actor: String): String =
  // Be careful: fhe stripMargin only works correctly if the variables being
  // substituted in do not contains characters that look like margins themselves.
    s"""
       |PREFIX  actor: <http://ontology.causeex.com/ontology/odps/Actor#>
       |PREFIX     gc: <http://ontology.causeex.com/ontology/odps/GeneralConcepts#>
       |
       |PREFIX   rdfs: <http://www.w3.org/2000/01/rdf-schema#>
       |
       |SELECT ?canonicalLabel
       |FROM <$host/$datasetName/data/$datasetName>
       |WHERE {
       |    ?subject a actor:$actor;
       |        gc:canonical_label ?canonicalLabel.
       |}
       |
       |#LIMIT 10
       |""".stripMargin

  def run(id: String, counter: Counter, bagger: Bagger[String], datasetName: String, connection: RDFConnection): Unit = {
    val query = mkSentenceQuery(datasetName, id)

    connection.queryResultSet(query, { resultSet =>
      while (resultSet.hasNext) {
        val querySolution = resultSet.next
        val canonicalLabel = querySolution.getLiteral("canonicalLabel").getString

        bagger.put(canonicalLabel)
        counter.inc()
      }
    })
  }

  def run(inputFile: File, outputFile: File): Unit = {
    Sourcer.sourceFromFile(inputFile).autoClose { source =>
      val lines = source.getLines.drop(1)

      new TsvWriter(FileUtils.printWriterFromFile(outputFile)).autoClose { tsvWriter =>
        tsvWriter.println("id", "count", "label", "comment")
        lines.foreach { line =>
          val Array(id, label, comment) = TsvUtils.readln(line)
          val counter = Counter()
          val bagger = new HashCodeBagger[String]

          Dataset.names.foreach { datasetName =>
            mkConnection(datasetName).autoClose { connection =>
              run(id, counter, bagger, datasetName, connection)
            }
          }
          new TsvWriter(FileUtils.printWriterFromFile(new File(outputDir + "/" + id + ".txt"))).autoClose { tsvWriter =>
            val keys = bagger.get()
            val countsAndKeys = keys.map { key => (bagger.get(key), key)}.sortBy { case (key, value) => (-key, value) }

            tsvWriter.println("count", "text")
            countsAndKeys.foreach { case (count, key) =>
              tsvWriter.println(count.toString, key)
            }
          }
          tsvWriter.println(id, counter.get.toString, label, comment)
        }
      }
    }
  }

  run(inputFile, outputFile)
}
