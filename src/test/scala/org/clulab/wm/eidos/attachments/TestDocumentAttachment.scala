package org.clulab.wm.eidos.attachments

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.time.LocalDateTime

import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.serialization.DocumentSerializer
import org.clulab.serialization.json._
import org.clulab.serialization.json.JSONSerializer
import org.clulab.timenorm.scate.SimpleInterval
import org.clulab.utils.Closer.AutoCloser
import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.document.DctDocumentAttachment
import org.clulab.wm.eidos.test.TestUtils._
import org.json4s.jackson.parseJson
import org.json4s.jackson.prettyJson
import org.json4s.jackson.renderJValue

class TestDocumentAttachment extends Test {

  def serialize(any: Any): Array[Byte] = {
    new ByteArrayOutputStream().autoClose { byteArrayOutputStream =>
      new ObjectOutputStream(byteArrayOutputStream).autoClose { objectOutputStream =>
        try {
          objectOutputStream.writeObject(any)
        }
        catch {
          case exception: Exception =>
            exception.printStackTrace()
            throw exception
        }
      }
      byteArrayOutputStream.toByteArray
    }
  }

  def deserialize[T](byteArray: Array[Byte]): T = {
    new ByteArrayInputStream(byteArray).autoClose { byteArrayInputStream =>
      new ObjectInputStream(byteArrayInputStream).autoClose { objectInputStream =>
        try {
          val res1 = objectInputStream.readObject()
          val res2 = res1.asInstanceOf[T]
          res2
        }
        catch {
          case exception: Exception =>
            exception.printStackTrace()
            throw exception
        }
      }
    }
  }

  "DctDocumentAttachment" should "serialize" in {
    val dct = DCT(SimpleInterval(LocalDateTime.now, LocalDateTime.now), "now")
    val oldDocumentAttachment = new DctDocumentAttachment(dct)
    val bytes = serialize(oldDocumentAttachment)
    val newDocumentAttachment: DctDocumentAttachment = deserialize(bytes)
    newDocumentAttachment should be (oldDocumentAttachment)

    val dct2 = DCT(SimpleInterval(LocalDateTime.now, LocalDateTime.now), "then")
    val oldDocumentAttachment2 = new DctDocumentAttachment(dct2)
    newDocumentAttachment should not be (oldDocumentAttachment2)
  }

  "Document with DctDocumentAttachment" should "serialize as text" in {
    val dct = DCT(SimpleInterval(LocalDateTime.now, LocalDateTime.now), "now")
    val oldDocument = new Document(Array.empty[Sentence])

    DctDocumentAttachment.setDct(oldDocument, dct)

    val documentSerializer = new DocumentSerializer()
    val documentString = documentSerializer.save(oldDocument)

    val newDocument = documentSerializer.load(documentString)
    val newDctOpt = DctDocumentAttachment.getDct(newDocument)
    newDctOpt.get should be (dct)

    val dct2 = DCT(SimpleInterval(LocalDateTime.now, LocalDateTime.now), "then")
    newDctOpt.get should not be (dct2)
  }

  "Document with DctDocumentAttachments" should "serialize as json" in {
    val dct = DCT(SimpleInterval(LocalDateTime.now, LocalDateTime.now), "now")
    val oldDocument = new Document(Array.empty[Sentence])

    DctDocumentAttachment.setDct(oldDocument, dct)

    val documentString = prettyJson(renderJValue(oldDocument.jsonAST))

    val newDocument: Document = JSONSerializer.toDocument(parseJson(documentString))
    val newDctOpt = DctDocumentAttachment.getDct(newDocument)
    newDctOpt.get should be (dct)

    val dct2 = DCT(SimpleInterval(LocalDateTime.now, LocalDateTime.now), "then")
    newDctOpt.get should not be (dct2)
  }
}
