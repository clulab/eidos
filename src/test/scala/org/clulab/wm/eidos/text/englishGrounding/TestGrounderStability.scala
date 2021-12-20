package org.clulab.wm.eidos.text.englishGrounding

import org.clulab.wm.eidos.serialization.jsonld.{JLDCorpus, JLDDeserializer}
import org.clulab.wm.eidos.test.EnglishGroundingTest
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser

import java.io.PrintWriter
import java.io.StringWriter

class TestGrounderStability extends EnglishGroundingTest {

  val ontologyHandler = ieSystem.components.ontologyHandlerOpt.get
  val deserializer = new JLDDeserializer()

  behavior of "Grounding"

  it should "survive a serialization round trip" in {
    val text = "Climate change: How global warming exacerbates conflict\n\n"
    val annotatedDocument = this.ieSystem.extractFromText(text)


    val jsonld1 = {
      val stringWriter = new StringWriter
      new PrintWriter(stringWriter).autoClose { stringWriter =>
        new JLDCorpus(annotatedDocument).serialize(stringWriter)
      }
      stringWriter.toString
    }

    val jsonld2 = {
      val json = jsonld1
      val corpus = deserializer.deserialize(json)
      val annotatedDocument = corpus.head
      annotatedDocument.allEidosMentions.foreach { eidosMention =>
        ontologyHandler.ground(eidosMention)
      }
      val stringWriter = new StringWriter
      new PrintWriter(stringWriter).autoClose { stringWriter =>
        new JLDCorpus(annotatedDocument).serialize(stringWriter)
      }
      stringWriter.toString
    }

    jsonld1 should be (jsonld2)
  }
}
