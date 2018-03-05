package org.clulab.wm.eidos.serialization

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import org.clulab.odin.{EventMention, Mention, SynPath}
import org.clulab.wm.eidos.Aliases.Quantifier
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.test.TestUtils.Test

class TestSerialization extends Test {

  val reader = new EidosSystem()

  behavior of "Standard Serializer"

  it should "serialize and deserialize again" in {
    val text = "Water trucking has decreased due to the cost of fuel."
    val annotatedDocument = reader.extractFrom(text)
    //    val mentionsOut = "testing"
    //    val mentionsOut: Quantifier = "testing"
    //    val mentionsOut = Seq("testing", "1, 2, 3")
    //    val mentionsOut = ("testing", Option(Seq("1", "2", "3")))
    //    val mentionsOut = new DerivedSerial("testing", Option(Seq("1", "2", "3")))
    //      val mentionsOut = Quantification("testing", Option(Seq("1", "2", "3")))
//    val prementionsOut = annotatedDocument.mentions(2).asInstanceOf[EventMention].paths // 0, 1 serializes but 2 doesn't
//    val mentionsOut = prementionsOut("cause")
    val mentionsOut = annotatedDocument.mentions
    // What is in here?

   // val mentionsOut: TestSerialization.SynPath = Seq((5, 4, "Testing"), (6, 7, "1, 2, 3"))
    //val mentionsOut: Map[String, Map[Mention, SynPath]] = Map()

    //    val mentionsOut: Scratch.TestType = Seq((1, 1, "help"), (2, 2, "me"))

    val streamOut = new ByteArrayOutputStream()
    val encoder = new ObjectOutputStream(streamOut)

    mentionsOut.indices.foreach { index =>
      val mention = mentionsOut(index)

      encoder.reset()
      try {
        encoder.writeObject(mentionsOut)
      }
      catch {
        case exception: Exception => println("Threw exception while writing index " + index)
      }

      val bytes = streamOut.toByteArray
      val streamIn = new ByteArrayInputStream(bytes)
      val decoder = new ObjectInputStream(streamIn)

      try {
        val mentionsIn = decoder.readObject()
      }
      catch {
        case exception: Exception => println("Threw exception while reading index " + index)
      }

      decoder.close()
    }
    encoder.close()

    //    val jValue1 = WMJSONSerializer.jsonAST(mentionsOut)
    //    val jValue2 = WMJSONSerializer.jsonAST(mentionsIn.asInstanceOf[Seq[Mention]])
    //
    //    val json1 = stringify(jValue1, pretty = true)
    //    val json2 = stringify(jValue2, pretty = true)
    //
    //    println(json1)
    //    println(json2)

    //    json1 should be (json2)
//    mentionsOut should be (mentionsIn)
  }
}

object TestSerialization {
  type SynPath = Seq[(Int, Int, String)] // governor, dependent, label

  @SerialVersionUID(1L)
  abstract class BaseSerial extends Serializable {
    //implicit val formats = org.json4s.DefaultFormats // This is the problem with serialization!
  }

  @SerialVersionUID(1L)
  case class DerivedSerial(quantifier: Quantifier, adverbs: Option[Seq[String]]) extends BaseSerial {

    // Serialization support
    //  def this() = this("".asInstanceOf[Quantifier], None)

    //  override def equals(other: Any) = {
    //    other match {
    //      case other: TestQuantification => quantifier == other.quantifier && adverbs == other.adverbs
    //      case _ => false
    //    }
    //  }
  }

  object DerivedSerial {
    def apply(whatever: String) = new DerivedSerial("hello", None)
  }

  object Scratch {
    type TestType = Seq[(Int, Int, String)] // governor, dependent, label
  }
}
