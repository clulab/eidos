package org.clulab.wm.eidos.apps

import java.time.LocalDateTime

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.AnnotatedDocument.Corpus
import org.clulab.wm.eidos.serialization.jsonld.JLDCorpus
import org.clulab.wm.eidos.serialization.jsonld.JLDDeserializer
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Timer

import scala.collection.Seq

// This began life in TestJLDDeserializer, but it is starting
// to be inappropriate there.
object RoundTrip extends App {
  class LocalException(message: String) extends Exception(message)

  val directoryName = args.headOption.getOrElse("../corpora/Doc52/txt")
  val ieSystem = new EidosSystem()
  val files = FileUtils.findFiles(directoryName, "txt")

  def newTitledAnnotatedDocument(text: String, title: String): AnnotatedDocument = {
    val documentCreationTime: Option[String] = Some(LocalDateTime.now().toString.take(10))
    val annotatedDocument = ieSystem.extractFromText(text, cagRelevantOnly = true,
        documentCreationTime, idOpt = None)

    annotatedDocument.document.id = Some(title)
    annotatedDocument
  }

  def serialize(corpus: Corpus): String = {
    val json = {
      val jldCorpus = new JLDCorpus(corpus)
      val jValue = jldCorpus.serialize()
      stringify(jValue, pretty = true)
    }

    json
  }

//  Timer.time("EidosPrimer") {
//    ieSystem.extractFromText("This is a test.")
//  }

  Timer.time("Whole thing") {
    files.par.foreach { file =>
      try {
        val text = FileUtils.getTextFromFile(file)

        val oldCorpus = Seq(newTitledAnnotatedDocument(text, file.getName))
        val oldJson = serialize(oldCorpus)

        val newCorpus = new JLDDeserializer().deserialize(oldJson)
        val newJson = serialize(newCorpus)

        {
          val oldLineCount = oldJson.count(_ == '\n')
          val newLineCount = newJson.count(_ == '\n')

          if (oldLineCount != newLineCount)
            throw new LocalException(s"Line count differs for file ${file.getName}: old = $oldLineCount and new = $newLineCount")
        }

        {
          val oldLength = oldJson.length
          val newlength = newJson.length

          if (oldLength != newlength)
            throw new LocalException(s"Length differs for file ${file.getName}: old = $oldLength and new = $newlength")
        }

        {
          if (oldJson != newJson)
            throw new LocalException(s"Content differs for file ${file.getName}: old = $oldJson and new = $newJson")
        }
      }
      catch {
        case exception: LocalException =>
          println(s"Local exception caught for file ${file.getName}:")
          exception.printStackTrace(System.out)
        case throwable: Throwable =>
          println(s"Unexpected exception caught for file ${file.getName}:")
          throwable.printStackTrace(System.out)
      }
    }
  }
}
