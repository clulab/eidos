package org.clulab.wm.eidos.apps.batch

import java.io.PrintWriter

import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.serialization.json.JLDDeserializer
import org.clulab.wm.eidos.serialization.json.JLDSerializer
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sinker

// This is borrowed from the sparql project and was pasted here so as not to impact Eidos yet.
object TsvUtils {
  val separatorChar = '\t'
  val separatorString: String = separatorChar.toString

  def escape(text: String): String = text
      .replace("\\", "\\\\")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")

  def quoted(text: String): String = '"' + text.replace("\"", "\"\"") + '"'

  def escapeExcel(text: String): String = {
    val escaped = escape(text)

    if (escaped.indexOf('"') >= 0) quoted(escaped)
    else escaped
  }

  class TsvWriter(printWriter: PrintWriter, isExcel: Boolean = true) {

    def println(strings: String*): Unit = {
      val escapedStrings =
        if (isExcel) strings.map(escapeExcel)
        else strings.map(escape)

      printWriter.println(escapedStrings.mkString(separatorString))
    }

    def close(): Unit = printWriter.close()
  }
}

object ReconstituteAndExportEstonia extends App {
  val inputDir = args(0)
  val outputFile = args(1)
  val threads = args(2).toInt

  val files = FileUtils.findFiles(inputDir, "jsonld")
  val serializer = new JLDSerializer(None)
  val deserializer = new JLDDeserializer()

  new TsvUtils.TsvWriter(Sinker.printWriterFromFile(outputFile)).autoClose { tsvWriter =>
    tsvWriter.println("docId", "className", "type", "subtype", "canonicalName", "mentionText", "sentenceText")

    files.foreach { file =>
      val json = FileUtils.getTextFromFile(file)
      val corpus = deserializer.deserialize(json)
      val docId = corpus.head.document.id.get
      val eidosMentions = corpus.head.allEidosMentions
      val sentences = corpus.head.document.sentences
      val jldCorpus = new JLDCorpus(corpus.head)

      eidosMentions.foreach { eidosMention =>
        val className = eidosMention.getClass.getSimpleName
        val jldExtraction = jldCorpus.newJLDExtraction(eidosMention, Map.empty)
        val jldType = jldExtraction.typeString
        val jldSubtype = jldExtraction.subtypeString
        val canonicalName = eidosMention.canonicalName
        val mentionText = eidosMention.odinMention.text
        val sentenceText = sentences(eidosMention.odinMention.sentence).getSentenceText

        tsvWriter.println(docId, className, jldType, jldSubtype, canonicalName, mentionText, sentenceText)
      }
    }
  }
}
