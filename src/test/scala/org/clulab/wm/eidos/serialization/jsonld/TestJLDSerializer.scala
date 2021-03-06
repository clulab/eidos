package org.clulab.wm.eidos.serialization.jsonld

import java.time.LocalDateTime

import org.clulab.odin.{CrossSentenceMention, TextBoundMention}
import org.clulab.serialization.json.stringify
import org.clulab.struct.{Interval => TextInterval}
import org.clulab.timenorm.scate.SimpleInterval
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.context.{DCT, GeoPhraseID, TimEx, TimeStep}
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.AnnotatedDocument.Corpus
import org.clulab.wm.eidos.document.attachments.DctDocumentAttachment
import org.clulab.wm.eidos.test.ExtractionTest
import org.clulab.wm.eidos.text.english.cag.CAG._

import scala.collection.Seq

class TestJLDSerializer extends ExtractionTest {

  def newTitledAnnotatedDocument(text: String): AnnotatedDocument = newTitledAnnotatedDocument(text, text)

  // TODO: Find a way, perhaps with a refiner, to add new or manipulate existing mentions
  // in the annotated document.  It should make use of this entrypoint:
  // protected def extractFromDoc(doc: Document, odinRefiners: Seq[Refiner[Mention]],
  //     eidosRefiners: Seq[Refiner[EidosMention]]): AnnotatedDocument = {

  def newTitledAnnotatedDocument(text: String, title: String): AnnotatedDocument = {
    val annotatedDocument = ieSystem.extractFromText(text)

    annotatedDocument.document.id = Some(title)
    annotatedDocument
  }
  
  def serialize(corpus: Corpus): String = {
    val json = {
      val jldCorpus = new JLDCorpus(corpus)
      val jValue = jldCorpus.serialize()
      stringify(jValue, pretty = true)
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
        Map("first" -> Seq(firstMention), "last" -> Seq(lastMention)),
        firstMention.document,
        true,
        "Found by me",
        Set.empty
      )
      val nextOdinMentions = crossSentenceMention +: prevOdinMentions
      val nextAnnotatedDocument = AnnotatedDocument(firstMention.document, nextOdinMentions)

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

  it should "serialize UTF-32 characters properly" in {
    val json = serialize(Seq(
      newTitledAnnotatedDocument("MEK1 binds ERK2\ud83d\udca9", "utf32")
    ))

    inspect(json)
    (json.indices.exists { index => json(index).isHighSurrogate && json(index + 1).isLowSurrogate }) should be (true)
    (json.indices.exists { index => json(index).isHighSurrogate && !json(index + 1).isLowSurrogate }) should be (false)
    (json.indices.exists { index => json(index).isLowSurrogate && !json(index - 1).isHighSurrogate }) should be (false)
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
    it should "not serialize unused time expressions" in {
      val json = serialize(Seq(
        newTitledAnnotatedDocument("2018-10-04", "This is an unused time expression")
      ))

      inspect(json)
      // Time expressions must now be involved in a mention.  These aren't.
      json.contains("timexes") should be(false)
      json.contains("intervals") should be(false)
      json.contains("TIMEX") should be(false)
    }
  }
  else
    println("It did not test unused time expressions")

  if (useTimeNorm) {
    it should "serialize used time expressions" in {
      val json = serialize(Seq(
        newTitledAnnotatedDocument("The drought of 2018-10-04 caused the famine of 2018-10-05.", "This is a used time expression")
      ))

      inspect(json)
      json.contains("timexes") should be(true)
      json.contains("intervals") should be(true)
      json.contains("TIMEX") should be(true)
    }
  }
  else
    println("It did not test used time expressions")

  if (useTimeNorm) {
    it should "serialize DCTs" in {
      val json = serialize(Seq( {
        val annotatedDocument = ieSystem.extractFromText("There's not much text here", dctStringOpt = Some("2018-10-04"))

        annotatedDocument.document.id = Some("This is a test of DCT")
        annotatedDocument
      }))

      inspect(json)
      json.contains("dct") should be(true)
      json.contains("intervals") should be(false)
    }
  }
  else
    println("It did not test DCTs")

  if (useGeoNorm) {
    it should "not serialize unused geo expressions" in {
      val json = serialize(Seq(
        newTitledAnnotatedDocument("Ethiopia!", "This is an unused geo expression")
      ))

      inspect(json)
      // Time expressions must now be involved in a mention.  These aren't.
      json.contains("geolocs") should be(false)
      json.contains("LocationExp") should be(false)
    }
  }
  else
    println("It did not test unused geo expressions")

  if (useGeoNorm) {
    it should "serialize used geo expressions" in {
      val json = serialize(Seq(
        newTitledAnnotatedDocument("The drought caused the famine in Ethiopia.", "This is a used geo expression")
      ))

      inspect(json)
      json.contains("geolocs") should be(true)
      json.contains("LocationExp") should be(true)
    }
  }
  else
    println("It didn't do it")

  println("It did not test used geo expressions")

  it should "serialize all kinds of attachments" in {
    val annotatedDocument1 = newTitledAnnotatedDocument(
        "Rainfall causes wetness.",
        "Pithy quote"
    )
    val document = annotatedDocument1.document
    val textBoundMention = annotatedDocument1.odinMentions.collect { case odinMention: TextBoundMention => odinMention }.head
    val emptyMention = textBoundMention // .newWithoutAttachment(textBoundMention.attachments.head)

    val trigger = "trigger"
    val someQuantifications = Some(Seq("one", "two"))
    val geoPhraseID = GeoPhraseID("text", Some("Denmark"), 3, 5)
    val timEx = TimEx(TextInterval(3, 8), Seq(TimeStep(LocalDateTime.now, LocalDateTime.now.plusDays(1))), "text")
    val dct = DCT(SimpleInterval(LocalDateTime.now.minusHours(5), LocalDateTime.now), "text")

    DctDocumentAttachment.setDct(document, dct)

    val attachments = Seq(
      new Decrease(trigger, someQuantifications),
      new Increase(trigger, someQuantifications),
      new Quantification(trigger, someQuantifications),
      new Property(trigger, someQuantifications),
      new Hedging(trigger, someQuantifications),
      new Negation(trigger, someQuantifications),
      new PosChange(trigger, someQuantifications),
      new NegChange(trigger, someQuantifications),

      new Location(geoPhraseID),
      new Time(timEx),
      new DCTime(dct),
      new Score(4.5d)
    )

    val fullMention = attachments.foldLeft(emptyMention) { case (textBoundMention, attachment) => textBoundMention.newWithAttachment(attachment)}
    val odinMentions = Seq(fullMention)
    val annotatedDocument2 = AnnotatedDocument(document, odinMentions)
    val json = serialize(Seq(annotatedDocument2))

    inspect(json)
    json should not be empty
    json should include (s""""type" : "${Decrease.kind}",""")
    json should include (s""""type" : "${Increase.kind}",""")
    json should include (s""""type" : "${Quantification.kind}",""")
    json should include (s""""type" : "${Property.kind}",""")
    json should include (s""""type" : "${Hedging.kind}",""")
    json should include (s""""type" : "${Negation.kind}",""")
    json should include (s""""type" : "${PosChange.kind}",""")
    json should include (s""""type" : "${NegChange.kind}",""")
  }
}
