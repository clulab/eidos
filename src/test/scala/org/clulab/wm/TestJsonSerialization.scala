package org.clulab.wm

import CAG._
import TestUtils._

import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.serialization.json.JSONSerialization

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

class TestJsonSerialization extends Test {
  
  behavior of "Document"
  
  it should "serialize correctly" in {
    class JSONSentence(val sentence: Sentence) extends JSONSerialization {
      def jsonObject: JObject =
        ("sentence" ->
          ("@id" -> sentence.equivalenceHash) ~
          ("text" -> sentence.getSentenceText())
        )
      
      def jsonAST: JValue = render(jsonObject)
    }
    
    class JSONDocument(val document: Document) extends JSONSerialization {
      def jsonObject: JObject =
        ("document" ->
          ("@id" -> document.equivalenceHash) ~
          ("sentences" -> document.sentences.map(_.equivalenceHash).toList)
        ) ~
        ("sentences" -> document.sentences.map(sentence => new JSONSentence(sentence).jsonObject).toList)
      
      def jsonAST: JValue = render(jsonObject)
    }
    
    val mentions = extractMentions(p1s1 + " " + p1s2) // get a document somehow or another
    val document = mentions(0).document // assuming there are any mentions
    val jsonDocument = new JSONDocument(document)

    val json = jsonDocument.json(true)
    println(json)
  }
}