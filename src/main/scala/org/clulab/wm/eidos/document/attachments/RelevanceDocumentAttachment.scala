package org.clulab.wm.eidos.document.attachments

import org.clulab.processors.{Document, DocumentAttachment, DocumentAttachmentBuilderFromJson, DocumentAttachmentBuilderFromText}
import org.json4s.JsonDSL._
import org.json4s._

/**
 * The attachment is for recording the relevance score given by a sentence classifier.
 */
@SerialVersionUID(100L)
class RelevanceDocumentAttachmentBuilderFromText extends DocumentAttachmentBuilderFromText {

  def mkDocumentAttachment(serializedText: String): RelevanceDocumentAttachment = {
    new RelevanceDocumentAttachment(serializedText.toFloat)
  }
}

@SerialVersionUID(100L)
class RelevanceDocumentAttachmentBuilderFromJson extends DocumentAttachmentBuilderFromJson {

  def mkDocumentAttachment(relevanceValue: JValue): RelevanceDocumentAttachment = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    val relevanceScore = (relevanceValue \ "relevance").extract[String].toFloat

    new RelevanceDocumentAttachment(relevanceScore)
  }
}

class RelevanceDocumentAttachment(val relevanceScore: Float) extends DocumentAttachment { // Maybe with EidosAttachment for jsonld?

  override def documentAttachmentBuilderFromTextClassName: String = classOf[RelevanceDocumentAttachmentBuilderFromText].getName
  override def documentAttachmentBuilderFromJsonClassName: String = classOf[RelevanceDocumentAttachmentBuilderFromJson].getName

  override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[RelevanceDocumentAttachment]

    // This equal method seems to be safe. Confirm with Becky later.
    "%.4f".format(this.relevanceScore) == "%.4f".format(that.relevanceScore)
  }

  override def toDocumentSerializer: String = {

    "%.4f".format(relevanceScore)
  }

  override def toJsonSerializer: JValue = {
    "relevance" -> "%.4f".format(relevanceScore)
  }
}

object RelevanceDocumentAttachment {
  protected val Key = "relevance"

  def getRelevanceDocumentAttachment(doc: Document): Option[RelevanceDocumentAttachment] = {
    val documentAttachmentOpt = doc.getAttachment(Key)
    val relevanceDocumentAttachmentOpt = documentAttachmentOpt.map { documentAttachment =>
      documentAttachment.asInstanceOf[RelevanceDocumentAttachment]
    }

    relevanceDocumentAttachmentOpt
  }

  def getRelevance(doc: Document): Option[Float] = {
    val relevanceDocumentAttachmentOpt = getRelevanceDocumentAttachment(doc)
    val relevanceOpt = relevanceDocumentAttachmentOpt.map { relevanceDocumentAttachment =>
      relevanceDocumentAttachment.relevanceScore
    }

    relevanceOpt
  }

  def setRelevance(doc: Document, relevanceScore: Float): RelevanceDocumentAttachment = {
    val relevanceDocumentAttachment = new RelevanceDocumentAttachment(relevanceScore)

    doc.addAttachment(Key, relevanceDocumentAttachment)
    relevanceDocumentAttachment
  }
}
