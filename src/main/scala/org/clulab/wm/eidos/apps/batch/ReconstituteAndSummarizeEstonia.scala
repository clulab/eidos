package org.clulab.wm.eidos.apps.batch

import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.serialization.json.JLDDeserializer
import org.clulab.wm.eidos.serialization.json.JLDSerializer
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.Counter
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sinker
import org.clulab.wm.eidos.utils.StringUtils
import org.clulab.wm.eidos.utils.TsvWriter

object ReconstituteAndSummarizeEstonia extends App {
  val inputDir = args(0)
  val outputFile = args(1)

  val files = FileUtils.findFiles(inputDir, "jsonld")
  val serializer = new JLDSerializer()
  val deserializer = new JLDDeserializer()

  val expectedType = "relation"
  val expectedSubtype = "causation"

  val counters = files.map { file =>
    file.getName -> Counter()
  }.toMap

  new TsvWriter(Sinker.printWriterFromFile(outputFile)).autoClose { tsvWriter =>
    tsvWriter.println("docId", "type", "subtype", "eidosCount")

    files.foreach { file =>
      val json = FileUtils.getTextFromFile(file)
      val corpus = deserializer.deserialize(json)
      val eidosMentions = corpus.head.allEidosMentions
      val jldCorpus = new JLDCorpus(corpus.head)

      eidosMentions.foreach { eidosMention =>
        val jldExtraction = jldCorpus.newJLDExtraction(eidosMention, Map.empty)
        val jldType = jldExtraction.typeString
        val jldSubtype = jldExtraction.subtypeString

        if (jldType == expectedType && jldSubtype == expectedSubtype)
          counters(file.getName).inc()
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
