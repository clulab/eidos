package org.clulab.wm.eidos.serialization.json

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.AnnotatedDocument.Corpus
import org.clulab.wm.eidos.groundings.EidosAdjectiveGrounder
import org.clulab.wm.eidos.serialization.json.{JLDCorpus => JLDEidosCorpus}
import org.clulab.wm.eidos.test.TestUtils.ExtractionTest
import org.clulab.wm.eidos.text.english.cag.CAG._

import scala.collection.Seq

class TestJLDDeserializer extends ExtractionTest {
  val adjectiveGrounder = EidosAdjectiveGrounder.fromConfig(ieSystem.config.getConfig("adjectiveGrounder"))

  def newTitledAnnotatedDocument(text: String): AnnotatedDocument = newTitledAnnotatedDocument(text, text)
  
  def newTitledAnnotatedDocument(text: String, title: String): AnnotatedDocument = {
    val annotatedDocument = ieSystem.extractFromText(text, keepText = true)

    annotatedDocument.document.id = Some(title)
    annotatedDocument
  }
  
  def serialize(corpus: Corpus) = {
    val json = {
      val jldCorpus = new JLDEidosCorpus(corpus)
      val jValue = jldCorpus.serialize(adjectiveGrounder)
      stringify(jValue, true)
    }
    
    json
  }

  def inspect(string: String): Unit =
      if (false) println(string)
  
  behavior of "JLDDeserializer"

  it should "deserialize" in {
    val json = serialize(Seq(
        newTitledAnnotatedDocument(p1s1, "This is a test")
    ))

    val corpus = new JLDDeserializer().deserialize(json)
  }
}
