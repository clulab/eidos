package org.clulab.wm

import CAG._
import TestUtils._

import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.serialization.json.JSONSerialization
import org.clulab.struct.DirectedGraph

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

class TestJsonSerialization extends Test {
  
  behavior of "Document"
  
  it should "serialize correctly" in {
    abstract class JSONObjectProvider extends JSONSerialization {
      def toJObject: JObject
      
      def jsonAST: JValue = render(toJObject)
    }
    
    class JSONGraphMapPair(key: String, value: DirectedGraph[String]) extends JSONObjectProvider {
      def toJObject: JObject = null
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
        ("chunk" -> getOrNone(sentence.chunks)) ~
        ("dependencies" -> sentence.graphs.map { item =>
          item match {
            case (key: String, value: DirectedGraph[String]) => key
          }
        })
      }
    }
    
    class JSONSentence(sentence: Sentence, text: Option[String]) extends JSONObjectProvider {
      def toJObject: JObject =
        ("sentence" -> // TODO: Make this prettier
          ("@id" -> sentence.equivalenceHash) ~
          ("words" -> sentence.words.indices.map(index => new JSONWord(sentence, index, text).toJObject).toList) ~
          ("text" -> sentence.getSentenceText())
        )
    }
    
    class JSONDocument(document: Document) extends JSONObjectProvider {
      def toJObject: JObject =
        ("document" ->
          ("@id" -> document.id) ~
          ("sentences" -> document.sentences.map(_.equivalenceHash).toList)
        ) ~
        ("sentences" -> document.sentences.map(sentence => new JSONSentence(sentence, document.text).toJObject).toList)
    }
    
    val text = p1s1 + " " + p1s2
    val mentions = extractMentions(text) // get a document somehow or another
    
    val document = mentions(0).document // assuming there are any mentions
    document.text = Some(text)
    
    val jsonDocument = new JSONDocument(document)
    val json = jsonDocument.json(true)
    
    println(json)
  }
}