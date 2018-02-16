package org.clulab.wm

import CAG._
import TestUtils._

import org.clulab.odin.Mention
import org.clulab.serialization.json.stringify
import org.clulab.wm.Aliases.Quantifier
import org.clulab.wm.serialization.json.JLDObject.Corpus
import org.clulab.wm.serialization.json.JLDObject.Grounding
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
    val jldCorpus = new JLDCorpus(corpus, new this.TestEntityGrounder())
    val jValue = jldCorpus.serialize()
    
    stringify(jValue, true)
  }
  
  class TestEntityGrounder extends EntityGrounder {
    // This was copied from RAPShell.  It needs to be somewhere else
    def convertToAdjective(adverb: String): String = {
      // This code is bad!
      if (adverb.endsWith("ily")) {
        adverb.slice(0, adverb.length - 3) ++ "y"
      }
     // else
      adverb.slice(0, adverb.length - 2)
    }
    
    def ground(mention: Mention, quantifier: Quantifier): Grounding = {
      val system = TestUtils.system
      // TODO: Improve this
      val pseudoStemmed = if (quantifier.endsWith("ly")) convertToAdjective(quantifier) else quantifier
      val modelRow = system.grounder.getOrElse(pseudoStemmed, Map.empty[String, Double])
      val intercept = modelRow.get(EidosSystem.INTERCEPT)
      val mu = modelRow.get(EidosSystem.MU_COEFF)
      val sigma = modelRow.get(EidosSystem.SIGMA_COEFF)
      
      Grounding(intercept, mu, sigma)
    }
  }
  
  behavior of "Serializer"

  it should "serialize one simple document" in {
    val json = serialize(Seq(
//        newAnnotatedDocument("This is a test"), 
        newAnnotatedDocument("Rainfall significantly increases poverty."), 
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