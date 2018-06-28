package org.clulab.wm.eidos.serialization

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import org.clulab.odin.{EventMention, Mention, TextBoundMention}
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.test.TestUtils.Test
import org.apache.commons.io.input.ClassLoaderObjectInputStream
import org.clulab.wm.eidos.attachments.Negation

class TestSerialization extends Test {
  val reader = new EidosSystem()

  def serialize(original: Any, index: Int): Unit = {
    val streamOut = new ByteArrayOutputStream()
    val encoder = new ObjectOutputStream(streamOut)
    encoder.writeObject(original)
    encoder.close()

    val bytes = streamOut.toByteArray
    val streamIn = new ByteArrayInputStream(bytes)
    val decoder = new ClassLoaderObjectInputStream(getClass.getClassLoader, streamIn)
    val copy = decoder.readObject()
    decoder.close()

    if (original.isInstanceOf[Mention])
      require(original == copy)
  }
  
  behavior of "Standard Serializer"

  it should "serialize and deserialize mentions" in {
    val text = "Water trucking has decreased due to the cost of fuel."
    //val text = "Food shortages cause hunger."
    val annotatedDocument = reader.extractFromText(text)
    val mentionsOut = annotatedDocument.odinMentions

    val negation = new Negation("trigger", Some(Seq("mod1", "mod1")))

    mentionsOut.foreach {
        case tmpEventMention: EventMention =>
          // Add any extra attachments to test here.
          val eventMention = tmpEventMention.newWithAttachment(negation)

          var index = 0

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
        case tmpTextBoundMention: TextBoundMention =>
          val textBoundMention = tmpTextBoundMention.newWithAttachment(negation)

          var index = 0

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
