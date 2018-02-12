package org.clulab.wm

import CAG._
import TestUtils._

import org.clulab.wm.serialization.json.JSONDocument

class TestJsonSerialization extends Test {
  
  behavior of "Document"
  
  it should "serialize" in {
    val text = p1s1 + " " + p1s2 // just some random example text
    val mentions = extractMentions(text) // get a document somehow or another
    val document = mentions(0).document // assuming there are any mentions
    document.text = Some(text) // because otherwise document doesn't track it
    val jsonDocument = new JSONDocument(document, mentions)
    val json = jsonDocument.json(true)
    
    println(json) // put a human in the loop
    json should not be empty
  }
}