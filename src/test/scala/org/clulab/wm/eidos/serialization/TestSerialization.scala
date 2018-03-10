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
  
  behavior of "Standard Serializer"

  it should "serialize and deserialize mentions" in {
    //val text = "Water trucking has decreased due to the cost of fuel."
    val text = "Food shortages cause hunger."
    val annotatedDocument = reader.extractFromText(text)
    val mentionsOut = annotatedDocument.odinMentions

    mentionsOut.indices.foreach { index =>
      val mentionOut = mentionsOut(index)
     
      if (mentionOut.isInstanceOf[EventMention]) {
        println("Found an EventMention")
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
        serialize(eventMention, index); index += 1
      }
      
      if (mentionOut.isInstanceOf[TextBoundMention]) {
        println("Found a TextBoundMention")
        
        var index = 0
        
        serialize(mentionOut, index); index += 1
      }
    }
  }
  
  it should  "serialize similar objects" in {
    var index = 0
    
    val just: JustAClass = new JustAClass(Seq((1, 2, "simple")))
    serialize(just, index); index += 1
    
    val norm1: NormalClass = new NormalClass(Seq((3, 4, "normal")), Map("hello" -> Map("there" -> Seq((3, 4,  "one")), "here" -> Seq((5, 6, "two")))))
    serialize(norm1,  index); index += 1
    
    val rec1: RecursiveClass = new RecursiveClass(Seq((1, 2, "simple")), Map.empty)
    val rec2: RecursiveClass = new RecursiveClass(Seq((1, 2, "simple")), Map("indirection" -> Map(rec1 -> Seq((1, 2, "simple")), rec1 -> Seq((1, 2, "simple")))))
    val rec3: RecursiveClass = new RecursiveClass(Seq((1, 2, "simple")), Map("doubleindirection" -> Map(rec1 ->Seq((1, 2, "simple")), rec2 -> Seq((1, 2, "simple")))))
    serialize(rec1, index); index += 1
    
    serialize(Seq(just, norm1, rec1, rec1), index); index += 1
  }

  encoder.close()
}
