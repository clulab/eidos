package org.clulab.wm.eidos.document.attachments

import org.clulab.processors.Document
import org.clulab.processors.DocumentAttachment
import org.clulab.processors.DocumentAttachmentBuilderFromJson
import org.clulab.processors.DocumentAttachmentBuilderFromText
import org.json4s.JsonDSL._
import org.json4s._

@SerialVersionUID(100L)
class TitleDocumentAttachmentBuilderFromText extends DocumentAttachmentBuilderFromText {

  def mkDocumentAttachment(serializedText: String): TitleDocumentAttachment = {
    new TitleDocumentAttachment(serializedText)
  }
}

@SerialVersionUID(100L)
class TitleDocumentAttachmentBuilderFromJson extends DocumentAttachmentBuilderFromJson {

  def mkDocumentAttachment(titleValue: JValue): TitleDocumentAttachment = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    val title = (titleValue \ "title").extract[String]

    new TitleDocumentAttachment(title)
  }
}

class TitleDocumentAttachment(val title: String) extends DocumentAttachment {

  override def documentAttachmentBuilderFromTextClassName: String = classOf[TitleDocumentAttachmentBuilderFromText].getName
  override def documentAttachmentBuilderFromJsonClassName: String = classOf[TitleDocumentAttachmentBuilderFromJson].getName

  override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[TitleDocumentAttachment]

    this.title == that.title
  }

  override def toDocumentSerializer: String = {
    title
  }

  override def toJsonSerializer: JValue = {
    "title" -> title
  }
}

object TitleDocumentAttachment {
  protected val Key = "title"

  def getDocumentAttachment(doc: Document): Option[TitleDocumentAttachment] = {
    val documentAttachmentOpt = doc.getAttachment(Key)
    val titleDocumentAttachmentOpt = documentAttachmentOpt.map { documentAttachment =>
      documentAttachment.asInstanceOf[TitleDocumentAttachment]
    }

    titleDocumentAttachmentOpt
  }

  def getTitle(doc: Document): Option[String] = {
    val titleDocumentAttachmentOpt = getDocumentAttachment(doc)
    val titleOpt = titleDocumentAttachmentOpt.map { titleDocumentAttachment =>
      titleDocumentAttachment.title
    }

    titleOpt
  }

  def setTitle(doc: Document, title: String): TitleDocumentAttachment = {
    val titleDocumentAttachment = new TitleDocumentAttachment(title)

    doc.addAttachment(Key, titleDocumentAttachment)
    titleDocumentAttachment
  }
}
