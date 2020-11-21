package org.clulab.wm.eidos.serialization

import java.io.{ByteArrayOutputStream, ObjectOutputStream}

import com.typesafe.config.Config
import org.clulab.processors.Document
import org.clulab.serialization.DocumentSerializer
import org.clulab.serialization.json.{DocOps, JSONSerializer, stringify}
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.groundings.grounders.{AdjectiveGrounder, EidosAdjectiveGrounder}
import org.clulab.wm.eidos.serialization.jsonld.{JLDCorpus, JLDDeserializer}
import org.clulab.wm.eidos.test.TestUtils.Test
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.{Canonicalizer, FileUtils}
import org.json4s.jackson.JsonMethods.{parse, pretty, render}

class TestDocSerialization extends Test {
  val config: Config = this.defaultConfig // Do not use EidosSystem's defaultConfig!
  val reader: EidosSystem = new EidosSystem(config)
  val adjectiveGrounder: AdjectiveGrounder = EidosAdjectiveGrounder.fromEidosConfig(config)
  val canonicalizer: Canonicalizer = reader.components.ontologyHandler.canonicalizer

  def testObjectSerialization(annotatedDocument: AnnotatedDocument): Unit = {
    val document = annotatedDocument.document
    val text = document.text.get

    behavior of "Java serializer"

    it should s"""process "$text" properly""" in {

      def serialize(original: Document): Unit = {
        val serial = new ByteArrayOutputStream().autoClose { streamOut =>
          new ObjectOutputStream(streamOut).autoClose { encoder =>
            encoder.writeObject(original)
          }
          streamOut.toByteArray
        }
        val copy = FileUtils.load[Document](serial, this)

        copy should not be (None)
//      copy should be (original)
//      document.hashCode should be (copy.hashCode)
      }

      serialize(document)
    }
  }

  def testJsonSerialization(annotatedDocument: AnnotatedDocument): Unit = {
    val document = annotatedDocument.document
    val text = document.text.get

    behavior of "JSON serializer"

    it should s"""process "$text" properly""" in {

      def serialize(original: Document): Unit = {
        val serial = pretty(render(original.jsonAST))
        val copy = JSONSerializer.toDocument(parse(serial))

        copy should not be (None)
//        copy should be (original)
//        document.hashCode should be (copy.hashCode)
      }

      serialize(document)
    }
  }

  def testCusomSerialization(annotatedDocument: AnnotatedDocument): Unit = {
    val document = annotatedDocument.document
    val text = document.text.get

    behavior of "Custom serializer"

    it should s"""process "$text" properly""" in {

      def serialize(original: Document): Unit = {
        val serializer = new DocumentSerializer
        val serial = serializer.save(original, encoding = "UTF-8", keepText = true)
        val copy = serializer.load(serial)

        copy should not be (None)
//        copy should be (original)
//        document.hashCode should be (copy.hashCode)
      }

      serialize(document)
    }
  }

  def testJldSerialization(annotatedDocument: AnnotatedDocument): Unit = {
    val text = annotatedDocument.document.text.get

    behavior of "JLD serializer"

    it should s"""process "$text" properly""" in {

      def serialize(original: AnnotatedDocument): Unit = {
        val corpus = Seq(original)
        val jldCorpus = new JLDCorpus(corpus)
        val jValue = jldCorpus.serialize()
        val json = stringify(jValue, pretty = true)
        val copy = new JLDDeserializer().deserialize(json)

        copy should not be (None)
//        copy should be (original)
//        annotatedDocument.document.hashCode should be (copy.head.document.hashCode)
      }

      serialize(annotatedDocument)
    }
  }

  val texts = Seq(
    "Water trucking has decreased due to the cost of fuel last week.",
    "300 refugees fled South Sudan; they left the country for Ethiopia. They left in 1997."
  )
  val annotateDocuments = texts.map(reader.extractFromText(_))

  annotateDocuments.foreach { annotatedDocument =>
    testObjectSerialization(annotatedDocument)
    testJsonSerialization(annotatedDocument)
    testCusomSerialization(annotatedDocument)
    testJldSerialization(annotatedDocument)
  }
}
