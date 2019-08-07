package org.clulab.wm.eidos.document

import java.time.LocalDateTime

import org.clulab.processors.Document
import org.clulab.processors.DocumentAttachment
import org.clulab.processors.DocumentAttachmentBuilderFromJson
import org.clulab.processors.DocumentAttachmentBuilderFromText
import org.clulab.timenorm.scate.SimpleInterval
import org.clulab.wm.eidos.context.DCT
import org.json4s._
import org.json4s.JsonDSL._

/**
 * In case anyone is wondering, DCT stands for Document Creation Time.
 */
@SerialVersionUID(100L)
class DctDocumentAttachmentBuilderFromText extends DocumentAttachmentBuilderFromText {

  def mkDocumentAttachment(serializedText: String): DctDocumentAttachment = {
    // See also the version in JLDDeserializer.
    val Array(text, start, end) = serializedText.split('\t')
    val startDateTime = LocalDateTime.parse(start)
    val endDateTime = LocalDateTime.parse(end)
    val interval = SimpleInterval(startDateTime, endDateTime)
    val dct = DCT(interval, text)

    new DctDocumentAttachment(dct)
  }
}

@SerialVersionUID(100L)
class DctDocumentAttachmentBuilderFromJson extends DocumentAttachmentBuilderFromJson {

  def mkDocumentAttachment(dctValue: JValue): DctDocumentAttachment = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    val text = (dctValue \ "text").extract[String]
    val start = (dctValue \ "start").extract[String]
    val end = (dctValue \ "end").extract[String]
    val dct = {
      val startDateTime = LocalDateTime.parse(start)
      val endDateTime = LocalDateTime.parse(end)
      val interval = SimpleInterval(startDateTime, endDateTime)
      val dct = DCT(interval, text)

      dct
    }

    new DctDocumentAttachment(dct)
  }
}

class DctDocumentAttachment(val dct: DCT) extends DocumentAttachment { // Maybe with EidosAttachment for jsonld?

  override def documentAttachmentBuilderFromTextClassName: String = classOf[DctDocumentAttachmentBuilderFromText].getName
  override def documentAttachmentBuilderFromJsonClassName: String = classOf[DctDocumentAttachmentBuilderFromJson].getName

  override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[DctDocumentAttachment]

    this.dct == that.dct
  }

  override def toDocumentSerializer: String = {
    val start = dct.interval.start
    val end = dct.interval.end

    dct.text + "\t" + start.toString + "\t" + end.toString
  }

  override def toJsonSerializer: JValue = {
    val start = dct.interval.start
    val end = dct.interval.end

    ("text" -> dct.text) ~
        ("start" -> start.toString) ~
        ("end" -> end.toString)
  }
}

object DctDocumentAttachment {
  protected val DctKey = "dct"

  def getDctDocumentAttachment(doc: Document): Option[DctDocumentAttachment] = {
    val documentAttachmentOpt = doc.getAttachment(DctKey)
    val dctDocumentAttachmentOpt = documentAttachmentOpt.map { documentAttachment =>
      documentAttachment.asInstanceOf[DctDocumentAttachment]
    }

    dctDocumentAttachmentOpt
  }

  def getDct(doc: Document): Option[DCT] = {
    val dctDocumentAttachmentOpt = getDctDocumentAttachment(doc)
    val dctOpt = dctDocumentAttachmentOpt.map { dctDocumentAttachment =>
      dctDocumentAttachment.dct
    }

    dctOpt
  }

  def setDct(doc: Document, dct: DCT): DctDocumentAttachment = {
    val dctDocumentAttachment = new DctDocumentAttachment(dct)

    doc.addAttachment(DctKey, dctDocumentAttachment)
    dctDocumentAttachment
  }
}
