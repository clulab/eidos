package org.clulab.wm.eidos.serialization.txt

import com.typesafe.config.Config
import org.clulab.processors.Document
import org.clulab.serialization.DocumentSerializer
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.groundings.grounders.{AdjectiveGrounder, EidosAdjectiveGrounder}
import org.clulab.wm.eidos.test.TestUtils.EidosTest
import org.clulab.wm.eidoscommon.Canonicalizer

class TestTxtSerialization extends EidosTest {
  val config: Config = this.defaultConfig // Do not use EidosSystem's defaultConfig!
  val reader: EidosSystem = new EidosSystem(config)
  val adjectiveGrounder: AdjectiveGrounder = EidosAdjectiveGrounder.fromEidosConfig(config)
  val canonicalizer: Canonicalizer = reader.components.ontologyHandlerOpt.get.canonicalizer


  def testCusomSerialization(annotatedDocument: AnnotatedDocument): Unit = {
    val document = annotatedDocument.document
    val text = document.text.get

    behavior of "Custom serializer"

    it should s"""process "$text" properly""" in {

      def serialize(original: Document): Unit = {
        val serializer = new DocumentSerializer
        val serial = serializer.save(original, encoding = "UTF-8", keepText = true)
        val copy = serializer.load(serial)

        copy should not be None
//        copy should be (original)
//        document.hashCode should be (copy.hashCode)
      }

      serialize(document)
    }
  }

  val texts = Seq(
    "Water trucking has decreased due to the cost of fuel last week.",
    "300 refugees fled South Sudan; they left the country for Ethiopia. They left in 1997."
  )
  val annotateDocuments: Seq[AnnotatedDocument] = texts.map(reader.extractFromText(_))

  annotateDocuments.foreach { annotatedDocument =>
    testCusomSerialization(annotatedDocument)
  }
}
