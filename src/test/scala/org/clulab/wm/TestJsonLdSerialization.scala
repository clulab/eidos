package org.clulab.wm

import CAG._
import TestUtils._

import org.clulab.serialization.json.stringify
import org.clulab.wm.serialization.json.JSONLDCorpus
import org.clulab.wm.serialization.json.JSONLDObject._
import org.clulab.wm.serialization.json.JSONLDPublisher

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
    val jsonldCorpus = new JSONLDCorpus(corpus)
    val jsonldPublisher = new JSONLDPublisher(jsonldCorpus)
    val jValue = jsonldPublisher.publish()
    val json = stringify(jValue, true)
    
    println(json)
    json should not be empty
  }
}