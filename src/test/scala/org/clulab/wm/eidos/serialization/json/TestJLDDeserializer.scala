package org.clulab.wm.eidos.serialization.json

import org.clulab.odin.CrossSentenceMention
import org.clulab.odin.Mention
import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.AnnotatedDocument.Corpus
import org.clulab.wm.eidos.groundings.EidosAdjectiveGrounder
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.serialization.json.{JLDCorpus => JLDEidosCorpus}
import org.clulab.wm.eidos.test.TestUtils.ExtractionTest
import org.clulab.wm.eidos.text.english.cag.CAG._
import org.clulab.wm.eidos.utils.Canonicalizer
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._

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

  class JLDReader {

    def readExtractions(jldExtractionsValue: JValue, documentId: String): (Seq[Mention], Seq[EidosMention]) = {
      (Seq.empty, Seq.empty)
    }

    def readDocument(jldDocumentValue: JValue): (Document, String) = {
      require(jldDocumentValue.isInstanceOf[JObject])
      val jldDocumentObject: JObject = jldDocumentValue.asInstanceOf[JObject]
      val objectType = jldDocumentObject \ "@type"
      require(objectType == JString("Document"))

      val documentIdValue = jldDocumentObject \ "@type"
      require(documentIdValue.isInstanceOf[JString])
      val documentId = documentIdValue.asInstanceOf[JString].values

      val titleValue = jldDocumentObject \ "title"
      require(documentIdValue.isInstanceOf[JString])
      val title = titleValue.asInstanceOf[JString].values

      val textValue = jldDocumentObject \ "text"
      require(textValue.isInstanceOf[JString])
      val text = titleValue.asInstanceOf[JString].values

      // Need to iterate through these now
      val sentences = Array.empty[Sentence]
      val document = new Document(sentences)
      document.id = Some(title)
      document.text = Some(text)

      (document, documentId)
    }

    def readCorpus(jldCorpusValue: JValue): Corpus = {
      require(jldCorpusValue.isInstanceOf[JObject])
      val jldCorpusObject: JObject = jldCorpusValue.asInstanceOf[JObject]
      val objectType = jldCorpusObject \ "@type"
      require(objectType == JString("Corpus"))

      val jldDocumentsValue: JValue = jldCorpusObject \ "documents"
      require(jldDocumentsValue.isInstanceOf[JArray])
      val jldDocumentsArray: JArray = jldDocumentsValue.asInstanceOf[JArray]
      val documentsAndDocumentIds = jldDocumentsArray.arr.map { jldDocument: JValue =>
        val documentAndDocumentId = readDocument(jldDocument)

        documentAndDocumentId
      }

      val jldExtractionsValue: JValue = jldCorpusObject \ "extractions"
      val annotatedDocuments = documentsAndDocumentIds.map { case (document, documentId) =>
        val (odinMentions: Seq[Mention], eidosMentions: Seq[EidosMention]) = readExtractions(jldExtractionsValue, documentId)
        val annotatedDocument = AnnotatedDocument(document, odinMentions, eidosMentions)

        annotatedDocument
      }
      val corpus = annotatedDocuments

      corpus
    }

    def read(json: String): Corpus = {
      val jValue: JValue = parse(json)
      val corpus = readCorpus(jValue)

      corpus
    }
  }

  def inspect(string: String): Unit =
      if (false) println(string)
  
  behavior of "JLDReader"

  it should "read" in {
    val json = serialize(Seq(
        newTitledAnnotatedDocument(p1s1, "This is a test")
    ))

    val corpus = new JLDReader().read(json)
  }
}
