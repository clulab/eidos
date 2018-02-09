package org.clulab.wm

import CAG._
import TestUtils._

import org.clulab.odin.Attachment
import org.clulab.odin.Mention
import org.clulab.odin.TextBoundMention
import org.clulab.odin.RelationMention
import org.clulab.odin.EventMention

import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.serialization.json.JSONSerialization
import org.clulab.struct.DirectedGraph
import org.clulab.struct.Interval
import org.clulab.wm.Aliases.Quantifier

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

class TestJsonSerialization extends Test {
  
  behavior of "Document"
  
  it should "serialize correctly" in {

    abstract class JSONObjectProvider extends JSONSerialization {
      
      def toJObject: JObject
      
      def jsonAST: JValue = render(toJObject)

      // TODO: Make these nicer by handing out ids from somewhere
      def id(any: Any): String = System.identityHashCode(any).toString
      
      // TODO: Documents may already have some kind of ID.  Should it be used?
      def id(document: Document): String = document.id.getOrElse(id(document.asInstanceOf[Any]))
      
      def newJSONMention(mention: Mention): JSONMention = mention match {
        case mention: TextBoundMention => new JSONTextBoundMention(mention)
        case mention: RelationMention => new JSONRelationMention(mention)
        case mention: EventMention => new JSONEventMention(mention)
      }      

      def newJSONAttachment(attachment: Attachment): JSONAttachment = attachment match {
        // These attachments are the first class in the matching package and could be changed
        case attachment: Quantification => new JSONAttachment("QUANT", attachment.quantifier, None)
        case attachment: Increase => new JSONAttachment("INC", attachment.trigger, attachment.quantifier)
        case attachment: Decrease => new JSONAttachment("DEC", attachment.trigger, attachment.quantifier)
      }
    }
    
    class JSONNamedArgument(name: String, attachments: Seq[Mention]) extends JSONObjectProvider {
      def toJObject: JObject = 
        ("name" -> name)
        ("attachments" -> attachments.map(newJSONMention(_).toJObject)) // TODO: Why not id?
    }
    
    class JSONAttachment(kind: String, text: String, modifiers: Option[Seq[Quantifier]]) extends JSONObjectProvider {
      
      def toJObject: JObject =
        ("type", kind) ~
        ("text", text) ~
        ("modifiers", modifiers) // TODO: This is as far as it goes
    }
    
    class JSONInterval(interval: Interval) extends JSONObjectProvider {
    
      def toJObject: JObject =
        ("start", interval.start) ~
        ("end", interval.end)
    }
    
    class JSONProvenance(mention: Mention) extends JSONObjectProvider {
      def toJObject: JObject =
        ("provenance" ->
          ("document" -> id(mention.document)) ~
          ("sentence" -> id(mention.sentenceObj)) ~
          ("positions" -> Seq(mention.tokenInterval).map(new JSONInterval(_).toJObject)) // TODO: We only have one
        )
    }

    class JSONMention(mention: Mention) extends JSONObjectProvider {
     
      def toJObject: JObject =
         ("@id" -> id(mention)) ~
         ("labels" -> mention.labels) ~
         ("text" -> mention.text) ~
         new JSONProvenance(mention).toJObject ~
         ("rule" -> mention.foundBy)
    }
    
    class JSONTextBoundMention(mention: TextBoundMention) extends JSONMention(mention) {
      
      override def toJObject: JObject =
         ("@type" -> "Entity") ~
         super.toJObject ~
         ("states" -> mention.attachments.map(newJSONAttachment(_).toJObject).toList)
    }

    class JSONRelationMention(mention: RelationMention) extends JSONMention(mention) {
      
      override def toJObject: JObject = 
        ("@type" -> "Undirected Relation") ~ // TODO: Is this true, see no trigger
        super.toJObject
        // TODO: No trigger
        // TODO: The picture is vague
        ("arguments", mention.arguments.map(item => new JSONNamedArgument(item._1, item._2).toJObject).toList)
    }

    class JSONEventMention(mention: EventMention) extends JSONMention(mention) {
      
      override def toJObject: JObject = {
        val sources = mention.arguments.getOrElse("cause", Nil) // TODO: are these correct names?
        val targets = mention.arguments.getOrElse("effect", Nil)
          
        ("@type" -> "Directed Relation") ~ // TODO: Is this true?
        super.toJObject ~
        ("trigger" -> newJSONMention(mention.trigger).toJObject) ~ // TODO: Why not just id?
        ("sources" -> sources.map(newJSONMention(_).toJObject)) ~ // TODO: Why not just id?
        ("destinations" -> targets.map(newJSONMention(_).toJObject)) // TODO: Why not just id?
      }
    }
    
    class JSONGraphMapPair(key: String, value: Option[DirectedGraph[String]]) extends JSONObjectProvider {
      
      def toJObject: JObject =
        ("dependencies" ->
          ("key" -> key) // TODO: How are these described?
        )
    }
    
    class JSONWord(sentence: Sentence, index: Int, text: Option[String]) extends JSONObjectProvider {
      
      def getOrNone(optionArray: Option[Array[String]]): Option[String] =
          if (optionArray.isDefined) Option(optionArray.get(index))
          else None
          
      def toJObject: JObject = {
        val startOffset = sentence.startOffsets(index)
        val endOffset = sentence.endOffsets(index)

        ("text" -> (if (text.isDefined) Option(text.get.substring(startOffset, endOffset)) else None)) ~
        ("tag" -> getOrNone(sentence.tags)) ~
        ("entity" -> getOrNone(sentence.entities)) ~
        ("startOffset" -> startOffset) ~
        ("endOffset" -> endOffset) ~
        ("lemma" -> getOrNone(sentence.lemmas)) ~
        ("chunk" -> getOrNone(sentence.chunks))
      }
    }
    
    class JSONSentence(sentence: Sentence, text: Option[String]) extends JSONObjectProvider {
      
      def toJObject: JObject = {
        val key = "universal-enhanced"
        val dependencies = sentence.graphs.get(key)
        
        ("@id" -> id(sentence)) ~
        ("words" -> sentence.words.indices.map(new JSONWord(sentence, _, text).toJObject).toList) ~
        ("text" -> sentence.getSentenceText()) ~
        new JSONGraphMapPair(key, dependencies).toJObject
      }
    }
    
    class JSONDocument(document: Document, mentions: Seq[Mention]) extends JSONObjectProvider {
      
      def toJObject: JObject =
        ("document" ->
          ("@id" -> document.id.getOrElse(id(document))) ~
          ("sentences" -> document.sentences.map(new JSONSentence(_, document.text).toJObject).toList)
        ) ~
        ("entities" -> mentions.map(newJSONMention(_).toJObject).toList)
    }
    
    val text = p1s1 + " " + p1s2
    val mentions = extractMentions(text) // get a document somehow or another
    
    val document = mentions(0).document // assuming there are any mentions
    document.text = Some(text)
    
    val jsonDocument = new JSONDocument(document, mentions)
    val json = jsonDocument.json(true)
    
    println(json)
  }
}