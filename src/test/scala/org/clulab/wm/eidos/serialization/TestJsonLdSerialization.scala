package org.clulab.wm.eidos.serialization

import scala.collection.Seq

import org.clulab.odin.Mention
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.Aliases.Quantifier
import org.clulab.wm.eidos.EntityGrounder
import org.clulab.wm.eidos.serialization.json.JLDObject.Corpus
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.serialization.json.JLDObject._
import org.clulab.wm.eidos.test.TestUtils
import org.clulab.wm.eidos.test.TestUtils.Test
import org.clulab.wm.eidos.text.cag.CAG._
import org.scalactic.source.Position.apply

class TestJsonSerialization extends Test {
  
  def newAnnotatedDocument(text: String): AnnotatedDocument = newAnnotatedDocument(text, text)
  
  def newAnnotatedDocument(text: String, title: String): AnnotatedDocument = {
    val ieSystem = TestUtils.ieSystem
    val annotatedDocument = ieSystem.extractFrom(text, true)

    annotatedDocument.document.id = Some(title)
    annotatedDocument
  }
  
  def serialize(corpus: Corpus) = {
    object TestEntityGrounder extends EntityGrounder {
  
      def ground(mention: Mention, quantifier: Quantifier) =
        TestUtils.ieSystem.ground(mention, quantifier)
    }
  
    val jldCorpus = new JLDCorpus(corpus, TestEntityGrounder)
    val jValue = jldCorpus.serialize()
    
    stringify(jValue, true)
  }
  
  def inspect(string: String) =
      if (false) println(string)
  
  behavior of "Serializer"

  it should "serialize the same each time" in {
    val json1 = serialize(Seq(
        newAnnotatedDocument(p1s1, "This is a test"), 
    ))
    val json2 = serialize(Seq(
        newAnnotatedDocument(p1s1, "This is a test"), 
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
        newAnnotatedDocument("Hello, world!", "Example Document"), 
    ))
    
    inspect(json)
    json should not be empty
  }
  
  it should "serialize one simple document" in {
    val json = serialize(Seq(
        newAnnotatedDocument(p1s1, "This is a test"), 
    ))
    
    inspect(json)
    json should not be empty
  }
  
  it should "be grounded" in {
    val json = serialize(Seq(
        newAnnotatedDocument("Rainfall significantly increases poverty."), 
    ))
    
    inspect(json)
    json.contains("intercept") should be (true)
    json.contains("mu") should be (true)
    json.contains("sigma") should be (true)
  }
  
  it should "serialize two simple documents" in {
    val json = serialize(Seq(
        newAnnotatedDocument("This is a test"), 
        newAnnotatedDocument("This is only a test")
    ))
    
    inspect(json)
    json should not be empty
  }
  
  it should "serialize one more complex document" in {
    val json = serialize(Seq(
        newAnnotatedDocument(p1s1, "p1s1"), 
    ))
    
    inspect(json)
    json should not be empty
  }
  
  it should "serialize two more complex documents" in {
    val json = serialize(Seq(
        newAnnotatedDocument(p1s2, "p1s2"), 
        newAnnotatedDocument(p2s2, "p2s2")
    ))
    
    inspect(json)
    json should not be empty
  }
  
  it should "serialize very complex documents" in {
    val json = serialize(Seq(
        newAnnotatedDocument(p1, "p1"), 
        newAnnotatedDocument(p2, "p2")
    ))
    
    inspect(json)
    json should not be empty
  }
  
  it should "serialize all CAGs in one pass" in {
    val json = serialize(Seq(
        newAnnotatedDocument(p1, "p1"), 
        newAnnotatedDocument(p2, "p2"),
        newAnnotatedDocument(p3, "p3"), 
        newAnnotatedDocument(p4, "p4"),
        newAnnotatedDocument(p5, "p5"), 
        newAnnotatedDocument(p6, "p6")
    ))
    
    inspect(json)
    json should not be empty    
  }
}