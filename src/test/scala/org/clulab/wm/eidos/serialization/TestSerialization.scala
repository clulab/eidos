package org.clulab.wm.eidos.serialization

import java.io.{ByteArrayOutputStream, ObjectOutputStream}

import org.clulab.odin.{EventMention, Mention, TextBoundMention}
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.test.TestUtils.Test
import org.clulab.wm.eidos.utils.{Closer, FileUtils}

class TestSerialization extends Test {
  val reader = new EidosSystem()

  def serialize(original: Any, index: Int): Unit = {
    val copy = Closer.autoClose(new ByteArrayOutputStream()) { streamOut =>
      Closer.autoClose(new ObjectOutputStream(streamOut)) { encoder =>
        encoder.writeObject(original)
      }

      val bytes = streamOut.toByteArray

      FileUtils.load[Any](bytes, this)
    }

    if (original.isInstanceOf[Mention])
      require(original == copy)
  }
  
  behavior of "Standard Serializer"

  it should "serialize and deserialize mentions" in {
    val text = "Water trucking has decreased due to the cost of fuel last week." // "last week" added for time
    //val text = "Food shortages cause hunger."
    val annotatedDocument = reader.extractFromText(text)
    val mentionsOut = annotatedDocument.odinMentions

    mentionsOut.foreach {
        case eventMention: EventMention =>
          var index = 0 // test

          serialize(eventMention.labels, index)
          index += 1
          serialize(eventMention.tokenInterval, index)
          index += 1
          serialize(eventMention.trigger, index)
          index += 1
          serialize(eventMention.arguments, index)
          index += 1
          serialize(eventMention.paths, index)
          index += 1
          serialize(eventMention.sentence, index)
          index += 1
          serialize(eventMention.document, index)
          index += 1
          serialize(eventMention.keep, index)
          index += 1
          serialize(eventMention.foundBy, index)

          eventMention.attachments.foreach { attachment =>
            serialize(attachment, index); index += 1
          }

          serialize(eventMention.attachments, index)
          index += 1
          serialize(eventMention, index)
          index += 1
        case textBoundMention: TextBoundMention =>
          var index = 0 // test

          textBoundMention.attachments.foreach { attachment =>
            serialize(attachment, index); index += 1
          }

          serialize(textBoundMention.attachments, index)
          index += 1
          val hash = textBoundMention.hashCode
          val mention2 = textBoundMention.copy()
          val hash2 = mention2.hashCode
          serialize(mention2, index)
          index += 1
    }
  }
}
