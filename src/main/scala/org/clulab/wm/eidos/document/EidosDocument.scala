package org.clulab.wm.eidos.document

import java.time.LocalDateTime

import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.timenorm.formal.{Interval => TimExInterval}
import org.clulab.struct.{Interval => TextInterval}
import org.clulab.timenorm.neural.TimeExpression
import org.json4s._
import org.json4s.JsonDSL._


@SerialVersionUID(1L)
case class TimeStep(startDateOpt: Option[LocalDateTime], endDateOpt: Option[LocalDateTime], duration: Long)
@SerialVersionUID(1L)
case class TimEx(span: TextInterval, intervals: Seq[TimeStep], text: String)
@SerialVersionUID(1L)
case class DCT(interval: TimExInterval, text: String)

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
    val startOpt = (dctValue \ "start").extractOpt[String]
    val endOpt = (dctValue \ "end").extractOpt[String]
    val dct =
        if (startOpt.isEmpty && endOpt.isEmpty) DCT(UnknownInterval, text)
        else {
          val start = startOpt.getOrElse(endOpt.get)
          val end = endOpt.getOrElse(startOpt.get)
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

