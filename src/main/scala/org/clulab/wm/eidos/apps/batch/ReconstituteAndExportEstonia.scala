package org.clulab.wm.eidos.apps.batch

import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.serialization.json.JLDDeserializer
import org.clulab.wm.eidos.serialization.json.JLDSerializer
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sinker
import org.clulab.wm.eidos.utils.TsvWriter

object ReconstituteAndExportEstonia extends App {
  val inputDir = args(0)
  val outputFile = args(1)

  val files = FileUtils.findFiles(inputDir, "jsonld")
  val serializer = new JLDSerializer
  val deserializer = new JLDDeserializer

  new TsvWriter(Sinker.printWriterFromFile(outputFile)).autoClose { tsvWriter =>
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
