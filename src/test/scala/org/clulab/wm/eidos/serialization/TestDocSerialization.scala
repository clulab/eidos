package org.clulab.wm.eidos.serialization

import java.io.{ByteArrayOutputStream, ObjectOutputStream}

import org.clulab.processors.Document
import org.clulab.serialization.DocumentSerializer
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.test.TestUtils.Test
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.serialization.json.{DocOps, JSONSerializer}
import org.json4s.jackson.JsonMethods.{parse, pretty, render}

class TestDocSerialization extends Test {
  val text = "Water trucking has decreased due to the cost of fuel last week." // "last week" added for time
  val reader = new EidosSystem()
  val document = reader.extractFromText(text).document

  behavior of "Java serializer"

  it should "serialize and deserialize documents" in {

    def serialize(original: Document): Unit = {
      val serial = (new ByteArrayOutputStream()).autoClose { streamOut =>
        (new ObjectOutputStream(streamOut)).autoClose { encoder =>
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

  behavior of "JSON serializer"

  it should "serialize and deserialize documents" in {

    def serialize(original: Document): Unit = {
      val serial = pretty(render(original.jsonAST))
      val copy = JSONSerializer.toDocument(parse(serial))

      copy should not be (None)
    }

    serialize(document)
  }

  behavior of "Custom serializer"

  it should "serialize and deserialize documents" in {

    def serialize(original: Document): Unit = {
      val serializer = new DocumentSerializer
      val serial = serializer.save(original, "UTF-8", true)
      val copy = serializer.load(serial)

      copy should not be (None)
    }

    serialize(document)
  }
}
