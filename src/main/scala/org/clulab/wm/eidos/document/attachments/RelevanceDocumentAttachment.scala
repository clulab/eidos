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
    val relevanceScoreStrings = serializedText.split('\t')
    new RelevanceDocumentAttachment(relevanceScoreStrings.map{x => x.toFloat})
  }
}

@SerialVersionUID(100L)
class RelevanceDocumentAttachmentBuilderFromJson extends DocumentAttachmentBuilderFromJson {

  def mkDocumentAttachment(relevanceValue: JValue): RelevanceDocumentAttachment = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    val relevanceScoreString = (relevanceValue \ "relevanceScores").extract[String]

    new RelevanceDocumentAttachment(relevanceScoreString.split("\t").map{x => x.toFloat})
  }
}

class RelevanceDocumentAttachment(val relevanceScores: Seq[Float]) extends DocumentAttachment { // Maybe with EidosAttachment for jsonld?

  override def documentAttachmentBuilderFromTextClassName: String = classOf[RelevanceDocumentAttachmentBuilderFromText].getName
  override def documentAttachmentBuilderFromJsonClassName: String = classOf[RelevanceDocumentAttachmentBuilderFromJson].getName

  override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[RelevanceDocumentAttachment]

    // This equal method seems to be safe. Confirm with Becky later.
    val thisRelScores = this.relevanceScores.map{x => "%.4f".format(x)}
    val thatRelScores = that.relevanceScores.map{x => "%.4f".format(x)}

    thisRelScores == thatRelScores
  }

  override def toDocumentSerializer: String = {

    relevanceScores.map{x => "%.4f".format(x)}.mkString("\t")

  }

  override def toJsonSerializer: JValue = {
    "relevanceScores" -> relevanceScores.map{x => "%.4f".format(x)}.mkString("\t")
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

  def getRelevance(doc: Document): Option[Seq[Float]] = {
    val relevanceDocumentAttachmentOpt = getRelevanceDocumentAttachment(doc)
    val relevanceOpt = relevanceDocumentAttachmentOpt.map { relevanceDocumentAttachment =>
      relevanceDocumentAttachment.relevanceScores
    }

    relevanceOpt
  }

  def setRelevance(doc: Document, relevanceScores: Seq[Float]): RelevanceDocumentAttachment = {
    val relevanceDocumentAttachment = new RelevanceDocumentAttachment(relevanceScores)

    doc.addAttachment(Key, relevanceDocumentAttachment)
    relevanceDocumentAttachment
  }
}
