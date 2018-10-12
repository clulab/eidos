package org.clulab.wm.eidos.serialization.json

import org.clulab.odin.{Attachment, CrossSentenceMention, Mention}
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.AnnotatedDocument
import org.clulab.wm.eidos.EidosSystem.Corpus
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.serialization.json.{JLDCorpus => JLDEidosCorpus}
import org.clulab.wm.eidos.test.TestUtils
import org.clulab.wm.eidos.test.TestUtils.Test
import org.clulab.wm.eidos.text.cag.CAG._
import org.clulab.wm.eidos.utils.Canonicalizer

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
    val json = {
      val jldCorpus = new JLDEidosCorpus(corpus, TestUtils.ieSystem)
      val jValue = jldCorpus.serialize()
      stringify(jValue, true)
    }
    
    json
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

  it should "work with cross-sentence mentions" in {

    def addCrossSentenceMention(prevAnnotatedDocument: AnnotatedDocument): AnnotatedDocument = {
      val prevOdinMentions = prevAnnotatedDocument.odinMentions
      val firstMention = prevOdinMentions.head
      val lastMention = prevOdinMentions.last
      val crossSentenceMention = new CrossSentenceMention(
        Seq("Coreference", "label1", "label2", "...", "labelN"),
        firstMention,
        lastMention,
        Map(("first" -> Seq(firstMention)), ("last" -> Seq(lastMention))),
        firstMention.document,
        true,
        "Found by me",
        Set.empty
      )
      val nextOdinMentions = crossSentenceMention +: prevOdinMentions
      val nextEidosMentions = EidosMention.asEidosMentions(nextOdinMentions, new Canonicalizer(TestUtils.ieSystem.loadableAttributes.stopwordManager), TestUtils.ieSystem)
      val nextAnnotatedDocument = AnnotatedDocument(firstMention.document, nextOdinMentions, nextEidosMentions)

      nextAnnotatedDocument
    }

    val prevAnnotatedDocument = newTitledAnnotatedDocument(p1, "p1")
    val nextAnnotatedDocument = addCrossSentenceMention(prevAnnotatedDocument)
    val json = serialize(Seq(nextAnnotatedDocument))

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

  if (useTimeNorm) {
    it should "serialize time expression" in {
      val json = serialize(Seq(
        newTitledAnnotatedDocument("2018-10-04", "This is a time expression")
      ))

      inspect(json)
      json.contains("timexes") should be(true)
      json.contains("intervals") should be(true)
    }
  }
  else
    println("It didn't do it")
}
