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

    //val relevanceScoreString = (relevanceValue \ "relevanceScores").extract[Seq[String]]
    val relevanceScoreString = (relevanceValue).extract[Seq[String]]


    new RelevanceDocumentAttachment(relevanceScoreString.map{x => x.toFloat})
  }
}

class RelevanceDocumentAttachment(val relevanceScores: Seq[Float]) extends DocumentAttachment { // Maybe with EidosAttachment for jsonld?

  override def documentAttachmentBuilderFromTextClassName: String = classOf[RelevanceDocumentAttachmentBuilderFromText].getName
  override def documentAttachmentBuilderFromJsonClassName: String = classOf[RelevanceDocumentAttachmentBuilderFromJson].getName

  protected def round(relevance: Float): Float = "%.5f".format(relevance).toFloat

  override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[RelevanceDocumentAttachment]
    val thisRelScores = this.relevanceScores.map(round)
    val thatRelScores = that.relevanceScores.map(round)

    thisRelScores == thatRelScores
  }

  override def toDocumentSerializer: String = {
    relevanceScores.map{x => x.toString}.mkString("\t")
  }

  override def toJsonSerializer: JValue = {
    relevanceScores
  }
}

object RelevanceDocumentAttachment {
  protected val key = "relevanceScore"

  def getRelevanceDocumentAttachment(doc: Document): Option[RelevanceDocumentAttachment] = {
    val documentAttachmentOpt = doc.getAttachment(key)
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

    doc.addAttachment(key, relevanceDocumentAttachment)
    relevanceDocumentAttachment
  }

  def setRelevanceOpt(doc: Document, relevanceOpts: Seq[Option[Float]]): RelevanceDocumentAttachment = {
    val relevanceAny = relevanceOpts.exists(_.isDefined)
    val relevanceAll = relevanceOpts.forall(_.isDefined)
    assert(!relevanceAny || relevanceAll)
    if (relevanceAll) {
      val relevances = relevanceOpts.map(_.get)
      setRelevance(doc, relevances)
    }
    else new RelevanceDocumentAttachment(Seq.empty)
  }
}
