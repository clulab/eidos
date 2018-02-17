package org.clulab.wm

import CAG._
import TestUtils._

import org.clulab.odin.Mention
import org.clulab.serialization.json.stringify
import org.clulab.wm.Aliases.Quantifier
import org.clulab.wm.serialization.json.JLDObject.Corpus
import org.clulab.wm.serialization.json.JLDCorpus
import org.clulab.wm.serialization.json.JLDObject._
import org.clulab.wm.serialization.json.JLDSerializer

class TestJsonSerialization extends Test {
  
  def newAnnotatedDocument(text: String): AnnotatedDocument = newAnnotatedDocument(text, text)
  
  def newAnnotatedDocument(text: String, title: String): AnnotatedDocument = {
    val system = TestUtils.system
    val mentions = system.extractFrom(text, true)
    val document = mentions(0).document
    
    document.id = Some(title)
    
    new AnnotatedDocument(document, mentions)
  }
  
  def serialize(corpus: Corpus) = {
    object TestEntityGrounder extends EntityGrounder {
  
      def ground(mention: Mention, quantifier: Quantifier) =
        TestUtils.system.ground(mention, quantifier)
    }
  
    val jldCorpus = new JLDCorpus(corpus, TestEntityGrounder)
    val jValue = jldCorpus.serialize()
    
    stringify(jValue, true)
  }
  
  behavior of "Serializer"

  it should "serialize one simple document" in {
    val json = serialize(Seq(
        newAnnotatedDocument(p1s1, "This is a test"), 
    ))
    val json2 = serialize(Seq(
        newAnnotatedDocument(p1s1, "This is a test"), 
    ))
    
    println(json)
    println(json2)
    json should not be empty
  }

  it should "as serialize one simple document" in {
    val json = serialize(Seq(
        newAnnotatedDocument(p1s1, "This is a test"), 
    ))
    
    println(json)
    json should not be empty
  }
  
  it should "be grounded" in {
    val json = serialize(Seq(
        newAnnotatedDocument("Rainfall significantly increases poverty."), 
    ))
    
    println(json)
    json.contains("intercept") should be (true)
    json.contains("mu") should be (true)
    json.contains("sigma") should be (true)
  }
  
  it should "serialize two simple documents" in {
    val json = serialize(Seq(
        newAnnotatedDocument("This is a test"), 
        newAnnotatedDocument("This is only a test")
    ))
    
    println(json)
    json should not be empty
  }
  
  it should "serialize one more complex document" in {
    val json = serialize(Seq(
        newAnnotatedDocument(p1s1, "p1s1"), 
    ))
    
    println(json)
    json should not be empty
  }
  
  it should "serialize two more complex documents" in {
    val json = serialize(Seq(
        newAnnotatedDocument(p1s2, "p1s2"), 
        newAnnotatedDocument(p2s2, "p2s2")
    ))
    
    println(json)
    json should not be empty
  }
  
  it should "serialize very complex documents" in {
    val json = serialize(Seq(
        newAnnotatedDocument(p1, "p1"), 
        newAnnotatedDocument(p2, "p2")
    ))
    
    println(json)
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
    
    println(json)
    json should not be empty    
  }
}