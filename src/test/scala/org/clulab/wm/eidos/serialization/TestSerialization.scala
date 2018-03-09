package org.clulab.wm.eidos.serialization

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import org.clulab.odin.{TextBoundMention, EventMention, Mention, SynPath}
import org.clulab.wm.eidos.Aliases.Quantifier
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.test.TestUtils.Test

@SerialVersionUID(1L)
class JustAClass(val text: SynPath) extends Serializable {
}

@SerialVersionUID(1L)
class NormalClass(val text: SynPath, val recursiveClasses: Map[String, Map[String, SynPath]]) extends Serializable {
}

@SerialVersionUID(1L)
class RecursiveClass(val text: SynPath, val recursiveClasses: Map[String, Map[RecursiveClass, SynPath]]) extends Serializable {
}

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

//    val mentionsOut: Seq[Map[String, Seq[String]]] = Seq(Map("a" -> Seq("a", "b", "c"), "b" -> Seq("asdf", "jkl;")))
    //val mentionsOut: Seq[Map[String, String]] = Seq(Map("a" -> "b", "c" -> "d"))
    // What is in here?

   // val mentionsOut: TestSerialization.SynPath = Seq((5, 4, "Testing"), (6, 7, "1, 2, 3"))
    //val mentionsOut: Map[String, Map[Mention, SynPath]] = Map()

    //    val mentionsOut: Scratch.TestType = Seq((1, 1, "help"), (2, 2, "me"))

    val streamOut = new ByteArrayOutputStream()
    val encoder = new ObjectOutputStream(streamOut)

    def serialize(any: Any, index: Int): Unit = {
      encoder.reset()
      try {
        encoder.writeObject(any)
        println("Success while writing index " + index)
      }
      catch {
        case exception: Exception => println("Threw exception while writing index " + index)
            exception.printStackTrace()
      }

      val bytes = streamOut.toByteArray
      val streamIn = new ByteArrayInputStream(bytes)
      val decoder = new ObjectInputStream(streamIn)

      try {
        val mentionsIn = decoder.readObject()
        println("Success while reading index " + index)
      }
      catch {
        case exception: Exception => println("Threw exception while reading index " + index)
      }
      decoder.close()
    }
    
    val just: JustAClass = new JustAClass(Seq((1, 2, "simple")))
    serialize(just, -3)
    
    val norm1: NormalClass = new NormalClass(Seq((3, 4, "normal")), Map("hello" -> Map("there" -> Seq((3, 4,  "one")), "here" -> Seq((5, 6, "two")))))
    serialize(norm1, -2)
    
    val rec1: RecursiveClass = new RecursiveClass(Seq((1, 2, "simple")), Map.empty)
    val rec2: RecursiveClass = new RecursiveClass(Seq((1, 2, "simple")), Map("indirection" -> Map(rec1 -> Seq((1, 2, "simple")), rec1 -> Seq((1, 2, "simple")))))
    val rec3: RecursiveClass = new RecursiveClass(Seq((1, 2, "simple")), Map("doubleindirection" -> Map(rec1 ->Seq((1, 2, "simple")), rec2 -> Seq((1, 2, "simple")))))
    serialize(rec1, -1);
    
    serialize(Seq(just, norm1, rec1, rec1), 0)

    mentionsOut.indices.foreach { index =>
      val mentionOut = mentionsOut(index)
     
      if (mentionOut.isInstanceOf[EventMention]) {
        val eventMention = mentionOut.asInstanceOf[EventMention]
        var index = 0

        serialize(eventMention.labels, index); index += 1
        serialize(eventMention.tokenInterval, index); index += 1
        serialize(eventMention.trigger, index); index += 1
        serialize(eventMention.arguments, index); index += 1
        eventMention.paths.foreach { case (key, value) =>
          println("See if the values work")
          serialize(value, index); index += 1
        }
        eventMention.paths.keys.foreach { key =>
          println("See if the parts work")
          val path = Map(key -> eventMention.paths(key))
          
          serialize(path, index); index += 1
        }
        println("See if a copy of path works")
        val path = Map() ++ eventMention.paths
        serialize(path, index); index += 1

        println("See if a copy of whole works")
        val copy = eventMention.copy(paths = Map() ++ eventMention.paths)
        serialize(copy, index); index += 1
        
        println("See if the whole works")
        serialize(eventMention.paths, index); index += 1 // This doesn't work!
        serialize(eventMention.sentence, index); index += 1
        serialize(eventMention.document, index); index += 1
        serialize(eventMention.keep, index); index += 1
        serialize(eventMention.foundBy, index); index += 1
        serialize(eventMention.attachments, index); index += 1
   //     serialize(eventMention, index); index += 1 // because recursive? not right constructor?
      }
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
