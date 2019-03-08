package org.clulab.wm.eidos.serialization.json

import org.clulab.odin.Mention
import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.document.AnnotatedDocument.Corpus
import org.clulab.wm.eidos.mentions.EidosMention
import org.json4s.JArray
import org.json4s.JObject
import org.json4s.JString
import org.json4s.JValue
import org.json4s.jackson.JsonMethods.parse

import scala.collection.Seq

class JLDDeserializer {

  def deserializeExtractions(jldExtractionsValue: JValue, documentId: String): (Seq[Mention], Seq[EidosMention]) = {
    (Seq.empty, Seq.empty)
  }

  def deserializeDocument(jldDocumentValue: JValue): (Document, String) = {
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

  def deserializeCorpus(jldCorpusValue: JValue): Corpus = {
    require(jldCorpusValue.isInstanceOf[JObject])
    val jldCorpusObject: JObject = jldCorpusValue.asInstanceOf[JObject]
    val objectType = jldCorpusObject \ "@type"
    require(objectType == JString("Corpus"))

    val jldDocumentsValue: JValue = jldCorpusObject \ "documents"
    require(jldDocumentsValue.isInstanceOf[JArray])
    val jldDocumentsArray: JArray = jldDocumentsValue.asInstanceOf[JArray]
    val documentsAndDocumentIds = jldDocumentsArray.arr.map { jldDocument: JValue =>
      val documentAndDocumentId = deserializeDocument(jldDocument)

      documentAndDocumentId
    }

    val jldExtractionsValue: JValue = jldCorpusObject \ "extractions"
    val annotatedDocuments = documentsAndDocumentIds.map { case (document, documentId) =>
      val (odinMentions: Seq[Mention], eidosMentions: Seq[EidosMention]) = deserializeExtractions(jldExtractionsValue, documentId)
      val annotatedDocument = AnnotatedDocument(document, odinMentions, eidosMentions)

      annotatedDocument
    }
    val corpus = annotatedDocuments

    corpus
  }

  def deserialize(json: String): Corpus = {
    val jValue: JValue = parse(json)
    val corpus = deserializeCorpus(jValue)

    corpus
  }
}
