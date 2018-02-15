package org.clulab.wm

import CAG._
import TestUtils._

import org.clulab.serialization.json.stringify
import org.clulab.wm.serialization.json.JLDCorpus
import org.clulab.wm.serialization.json.JLDObject._
import org.clulab.wm.serialization.json.JLDPublisher

class TestJsonSerialization extends Test {
  
  def newAnnotatedDocument(text: String, title: String): AnnotatedDocument = {
    val system = TestUtils.system
    val processor = system.proc
    val document = processor.annotate(text, true)
    val mentions = system.extractFrom(document)
    
    //document.id = Some(title)
    
    new AnnotatedDocument(document, mentions)
  }
  
  behavior of "Corpus"
  
  it should "serialize" in {
    val corpus = Seq(
        newAnnotatedDocument(p1s1 + " " + p1s2, "This is the first document"), 
        newAnnotatedDocument(p2s1 + " " + p2s2, "This is the second document")
    )
    val jldCorpus = new JLDCorpus(corpus)
    val jldPublisher = new JLDPublisher(jldCorpus)
    val jValue = jldPublisher.publish()
    val json = stringify(jValue, true)
    
    println(json)
    json should not be empty
  }
}