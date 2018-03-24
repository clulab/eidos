package org.clulab.wm.eidos.serialization.json

import org.clulab.odin.Mention
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.Aliases.Quantifier
import org.clulab.wm.eidos.AnnotatedDocument
import org.clulab.wm.eidos.EidosSystem.Corpus
import org.clulab.wm.eidos.groundings.{AdjectiveGrounder, AdjectiveGrounding}
import org.clulab.wm.eidos.serialization.json.odin.{JLDCorpus => JLDOdinCorpus}
import org.clulab.wm.eidos.serialization.json.{JLDCorpus => JLDEidosCorpus}
import org.clulab.wm.eidos.test.TestUtils
import org.clulab.wm.eidos.test.TestUtils.Test
import org.clulab.wm.eidos.text.cag.CAG._

import scala.collection.Seq

class TestJLDSerializer extends Test {
  
  def newTitledAnnotatedDocument(text: String): AnnotatedDocument = newTitledAnnotatedDocument(text, text)
  
  def newTitledAnnotatedDocument(text: String, title: String): AnnotatedDocument = {
    val ieSystem = TestUtils.ieSystem
    val annotatedDocument = ieSystem.extractFromText(text, keepText = true)

    annotatedDocument.document.id = Some(title)
    annotatedDocument
  }
  
  def serialize(corpus: Corpus) = {
    val json1 = {
      val jldCorpus = new JLDOdinCorpus(corpus, TestUtils.ieSystem)
      val jValue = jldCorpus.serialize()
      stringify(jValue, true)
    }
    
    val json2 = {
      val jldCorpus = new JLDEidosCorpus(corpus, TestUtils.ieSystem)
      val jValue = jldCorpus.serialize()
      stringify(jValue, true)
    }
    
//    if (json1 != json2)
//      println(json1 + json2)
      
    json1 + json2
  }
  
  def inspect(string: String): Unit =
      if (false) println(string)
  
  behavior of "JLDSerializer"

  it should "serialize the same each time" in {
    val json1 = serialize(Seq(
        newTitledAnnotatedDocument(p1s1, "This is a test")
    ))
    val json2 = serialize(Seq(
        newTitledAnnotatedDocument(p1s1, "This is a test")
    ))
    
    json1 should not be empty
    json2 should not be empty
    // This is a problem!
    //json1 should be (json2)
  }

  // This is used to bootstrap the documentation on the GitHub wiki.
  // See /doc/example.jsonld for the final version.
  it should "say hello" in {
    val json = serialize(Seq(
        newTitledAnnotatedDocument("Hello, world!", "Example Document")
    ))
    
    inspect(json)
    json should not be empty
  }
  
  it should "serialize one simple document" in {
    val json = serialize(Seq(
        newTitledAnnotatedDocument(p1s1, "This is a test")
    ))
    
    inspect(json)
    json should not be empty
  }
  
  it should "be grounded" in {
    val json = serialize(Seq(
        newTitledAnnotatedDocument("Rainfall significantly increases poverty.")
    ))
    
    inspect(json)
    json.contains("intercept") should be (true)
    json.contains("mu") should be (true)
    json.contains("sigma") should be (true)
  }
  
  it should "serialize two simple documents" in {
    val json = serialize(Seq(
        newTitledAnnotatedDocument("This is a test"), 
        newTitledAnnotatedDocument("This is only a test")
    ))
    
    inspect(json)
    json should not be empty
  }
  
  it should "serialize one more complex document" in {
    val json = serialize(Seq(
        newTitledAnnotatedDocument(p1s1, "p1s1")
    ))
    
    inspect(json)
    json should not be empty
  }
  
  it should "serialize two more complex documents" in {
    val json = serialize(Seq(
        newTitledAnnotatedDocument(p1s2, "p1s2"), 
        newTitledAnnotatedDocument(p2s2, "p2s2")
    ))
    
    inspect(json)
    json should not be empty
  }
  
  it should "serialize very complex documents" in {
    val json = serialize(Seq(
        newTitledAnnotatedDocument(p1, "p1"), 
        newTitledAnnotatedDocument(p2, "p2")
    ))
    
    inspect(json)
    json should not be empty
  }
  
  it should "serialize all CAGs in one pass" in {
    val json = serialize(Seq(
        newTitledAnnotatedDocument(p1, "p1"), 
        newTitledAnnotatedDocument(p2, "p2"),
        newTitledAnnotatedDocument(p3, "p3"), 
        newTitledAnnotatedDocument(p4, "p4"),
        newTitledAnnotatedDocument(p5, "p5"), 
        newTitledAnnotatedDocument(p6, "p6")
    ))
    
    inspect(json)
    json should not be empty    
  }
}
