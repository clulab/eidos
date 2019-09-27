package org.clulab.wm.eidos.document.attachments

import org.clulab.processors.Document
import org.clulab.processors.DocumentAttachment
import org.clulab.processors.DocumentAttachmentBuilderFromJson
import org.clulab.processors.DocumentAttachmentBuilderFromText
import org.json4s.JsonDSL._
import org.json4s._

@SerialVersionUID(100L)
class LocationDocumentAttachmentBuilderFromText extends DocumentAttachmentBuilderFromText {

  def mkDocumentAttachment(serializedText: String): LocationDocumentAttachment = {
    new LocationDocumentAttachment(serializedText)
  }
}

@SerialVersionUID(100L)
class LocationDocumentAttachmentBuilderFromJson extends DocumentAttachmentBuilderFromJson {

  def mkDocumentAttachment(locationValue: JValue): LocationDocumentAttachment = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    val location = (locationValue \ "location").extract[String]

    new LocationDocumentAttachment(location)
  }
}

class LocationDocumentAttachment(val location: String) extends DocumentAttachment {

  override def documentAttachmentBuilderFromTextClassName: String = classOf[LocationDocumentAttachmentBuilderFromText].getName
  override def documentAttachmentBuilderFromJsonClassName: String = classOf[LocationDocumentAttachmentBuilderFromJson].getName

  override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[LocationDocumentAttachment]

    this.location == that.location
  }

  override def toDocumentSerializer: String = {
    location
  }

  override def toJsonSerializer: JValue = {
    "location" -> location
  }
}

object LocationDocumentAttachment {
  protected val Key = "location"

  def getDocumentAttachment(doc: Document): Option[LocationDocumentAttachment] = {
    val documentAttachmentOpt = doc.getAttachment(Key)
    val locationDocumentAttachmentOpt = documentAttachmentOpt.map { documentAttachment =>
      documentAttachment.asInstanceOf[LocationDocumentAttachment]
    }

    locationDocumentAttachmentOpt
  }

  def getLocation(doc: Document): Option[String] = {
    val locationDocumentAttachmentOpt = getDocumentAttachment(doc)
    val locationOpt = locationDocumentAttachmentOpt.map { locationDocumentAttachment =>
      locationDocumentAttachment.location
    }

    locationOpt
  }

  def setLocation(doc: Document, location: String): LocationDocumentAttachment = {
    val locationDocumentAttachment = new LocationDocumentAttachment(location)

    doc.addAttachment(Key, locationDocumentAttachment)
    locationDocumentAttachment
  }
}
