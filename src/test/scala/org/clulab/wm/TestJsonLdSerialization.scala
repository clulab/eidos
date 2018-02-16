package org.clulab.wm

import CAG._
import TestUtils._

import org.clulab.serialization.json.stringify
import org.clulab.wm.serialization.json.JLDObject.Corpus
import org.clulab.wm.serialization.json.JLDCorpus
import org.clulab.wm.serialization.json.JLDObject._
import org.clulab.wm.serialization.json.JLDSerializer

class TestJsonSerialization extends Test {
  
  def newAnnotatedDocument(text: String): AnnotatedDocument = newAnnotatedDocument(text, text)
  
  def newAnnotatedDocument(text: String, title: String): AnnotatedDocument = {
    val system = TestUtils.system
    val processor = system.proc
    val document = processor.annotate(text, true)
    val mentions = system.extractFrom(document)
    
    document.id = Some(title)
    
    new AnnotatedDocument(document, mentions)
  }
  
  def serialize(corpus: Corpus) = {
    val jldCorpus = new JLDCorpus(corpus)
    val jValue = jldCorpus.serialize()
    
    stringify(jValue, true)
  }
  
  behavior of "Serializer"

  it should "serialize one simple document" in {
    val json = serialize(Seq(
        newAnnotatedDocument("This is a test"), 
    ))
    
    println(json)
    json should not be empty
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