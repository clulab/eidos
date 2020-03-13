package ai.lum.eidos.sparql.apps

import java.io.File

import ai.lum.eidos.sparql.utils.Closer.AutoCloser
import ai.lum.eidos.sparql.utils.FileUtils
import ai.lum.eidos.sparql.utils.Sourcer
import ai.lum.eidos.sparql.utils.TsvUtils.TsvWriter

import scala.collection.mutable.ArrayBuffer

object ReadActors extends App {
  case class Row(idOpt: Option[String], labelOpt: Option[String], commentOpt: Option[String])

  val inputFile = new File(args(0))
  val outputFile = new File(args(1))

  def between(prevValue: Option[String], line: String, prefix: String, suffix: String): Option[String] = {
    if (line.startsWith(prefix) && line.endsWith(suffix)) {
      val newValue = line.substring(prefix.length, line.length - suffix.length)

      Some(prevValue.map(_ + " " + newValue).getOrElse(newValue))
    }
    else
      prevValue
  }

  def between(prevValue: Option[String], line: String, prefix: String, suffixes: Seq[String]): Option[String] = {
    if (line.startsWith(prefix)) {
      val suffixIndex = suffixes.indexWhere { suffix => line.endsWith(suffix) }

      if (suffixIndex >= 0) {
        val newValue = line.substring(prefix.length, line.length - suffixes(suffixIndex).length)

        Some(prevValue.map(_ + " " + newValue).getOrElse(newValue))
      }
      else
        prevValue
    }
    else
      prevValue
  }

  val rows = Sourcer.sourceFromFile(inputFile).autoClose { source =>
    var inside = false
    var idOpt: Option[String] = None
    var labelOpt: Option[String] = None
    var commentOpt: Option[String] = None
    var rows: ArrayBuffer[Row] = ArrayBuffer.empty

    source.getLines.foreach { line =>
      if (inside) {
        if (line == "") {
          if (idOpt.nonEmpty || labelOpt.nonEmpty || commentOpt.nonEmpty)
            rows += Row(idOpt, labelOpt, commentOpt)
          inside = false
          idOpt = None
          labelOpt = None
          commentOpt = None
        }
        else {
            labelOpt = between(  labelOpt, line, "    rdfs:label \"",   Seq("\"@en ;", "\"@en ."))
          commentOpt = between(commentOpt, line, "    rdfs:comment \"", Seq("\"@en ;", "\"@en ."))
        }
      }
      else {
        idOpt = between(idOpt, line, ":", " a owl:Class ;")
        if (idOpt.isDefined)
          inside = true
      }
    }
    rows.toArray
  }

  new TsvWriter(FileUtils.printWriterFromFile(outputFile)).autoClose { tsvWriter =>
    tsvWriter.println("id", "label", "comment")
    rows.foreach { row =>
      val id = row.idOpt.getOrElse("")
      val label = row.labelOpt.getOrElse("")
      val command = row.commentOpt.getOrElse("")

      tsvWriter.println(id, label, command)
    }
  }
}
