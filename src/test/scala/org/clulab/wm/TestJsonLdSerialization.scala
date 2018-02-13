package org.clulab.wm

import CAG._
import TestUtils._

import org.clulab.serialization.json.stringify
import org.clulab.wm.serialization.json.JSONLDAnthology
import org.clulab.wm.serialization.json.JSONLDObjectProvider._
import org.clulab.wm.serialization.json.JSONLDPublisher

class TestJsonSerialization extends Test {
  
  behavior of "Document"
  
  it should "serialize" in {
    val system = TestUtils.system
    val processor = system.proc

    def newAnnotatedDocument(text: String, title: String): AnnotatedDocument = {
      val document = processor.annotate(text, true)
      val mentions = system.extractFrom(document)
      
      document.id = Some(title)
      
      new AnnotatedDocument(document, mentions)
    }
    
    val anthology = Seq(
        newAnnotatedDocument(p1s1 + " " + p1s2, "This is the first document"), 
        newAnnotatedDocument(p2s1 + " " + p2s2, "This is the second document")
    )
    val jsonldAnthology = new JSONLDAnthology(anthology)
    val jsonldPublisher = new JSONLDPublisher(jsonldAnthology)
    val jValue = jsonldPublisher.publish()
    val json = stringify(jValue, true)
    
    println(json)
    json should not be empty
  }
}