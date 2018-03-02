package org.clulab.wm.eidos.serialization.json

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream

import org.clulab.odin.Mention
import org.clulab.odin.EventMention
import org.clulab.serialization.json.JSONSerializer
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.Aliases.Quantifier
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments.Quantification
import org.clulab.wm.eidos.serialization.json.JLDObject._
import org.clulab.wm.eidos.test.TestUtils.Test
import org.clulab.wm.eidos.text.cag.CAG._

import scala.beans.BeanProperty

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
class TestJsonSerialization extends Test {
  
  val reader = new EidosSystem()

  behavior of "JSONSerializer"

  ignore should "serialize and deserialize again" in {
    val text = "Water trucking has decreased due to the cost of fuel."
    val annotatedDocument = reader.extractFrom(text)
    val mentionsOut = annotatedDocument.mentions
    val jValue1 = WMJSONSerializer.jsonAST(mentionsOut)
    val mentionsIn = WMJSONSerializer.toMentions(jValue1)
    val jValue2 = WMJSONSerializer.jsonAST(mentionsOut)
    val json1 = stringify(jValue1, pretty = true)
    val json2 = stringify(jValue2, pretty = true)

//    println(json1)
//    println(json2)

    json1 should be (json2)
//    mentionsOut should be (mentionsIn)
  }
  
  it should "not include duplicates" in {
    import org.clulab.wm.eidos.serialization.json.json.MentionOps
 
    val text = "The government promotes improved cultivar to boost agricultural production for ensuring food security."
    val annotatedDocument = reader.extractFrom(text)
    val mentionsOut = annotatedDocument.mentions
    val count = mentionsOut.size
    
    // Are any found by exact same rule?
    val allDoubles = mentionsOut.map(mention => (MentionOps.id(mention), mention.foundBy))
    val uniqueDoubles = allDoubles.distinct
    allDoubles.size should be (uniqueDoubles.size)

    // Are any found by different rules?
    val allSingles = mentionsOut.map(mention => MentionOps.id(mention))
    val uniqueSingles = allSingles.distinct
    allSingles.size should be (uniqueSingles.size)
    
    val jValue1 = WMJSONSerializer.jsonAST(mentionsOut)
    val json1 = stringify(jValue1, pretty = true)
    println(json1)
  }
  
  behavior of "Standard Serialzier"
  
  ignore should "serialize and deserialize again" in {
    val text = "Water trucking has decreased due to the cost of fuel."
    val annotatedDocument = reader.extractFrom(text)
//    val mentionsOut = "testing"
//    val mentionsOut: Quantifier = "testing"
//    val mentionsOut = Seq("testing", "1, 2, 3")
//    val mentionsOut = ("testing", Option(Seq("1", "2", "3")))
//    val mentionsOut = new DerivedSerial("testing", Option(Seq("1", "2", "3")))
//      val mentionsOut = Quantification("testing", Option(Seq("1", "2", "3")))
    val mentionsOut = annotatedDocument.mentions(2).asInstanceOf[EventMention].paths // 0, 1 serializes but 2 doesn't
    // What is in here?
    
//    val mentionsOut: Scratch.TestType = Seq((1, 1, "help"), (2, 2, "me"))
    
    val streamOut = new ByteArrayOutputStream()
    val encoder = new ObjectOutputStream(streamOut)
//    try {
    encoder.writeObject(mentionsOut)
//    } 
//    catch {
//      case exception: Exception => Nil
//    }
    encoder.close()
    
    val bytes = streamOut.toByteArray
    
    val streamIn = new ByteArrayInputStream(bytes)
    val decoder = new ObjectInputStream(streamIn)
    val mentionsIn = decoder.readObject()
    decoder.close()
    
//    val jValue1 = WMJSONSerializer.jsonAST(mentionsOut)
//    val jValue2 = WMJSONSerializer.jsonAST(mentionsIn.asInstanceOf[Seq[Mention]])
//    
//    val json1 = stringify(jValue1, pretty = true)
//    val json2 = stringify(jValue2, pretty = true)
//
//    println(json1)
//    println(json2)

//    json1 should be (json2)
    mentionsOut should be (mentionsIn)
  }
}