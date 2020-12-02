package org.clulab.wm.eidos.document

import org.clulab.processors.Document
import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.context.TimeNormFinder
import org.clulab.wm.eidos.document.attachments.DctDocumentAttachment
import org.clulab.wm.eidos.document.attachments.LocationDocumentAttachment
import org.clulab.wm.eidos.document.attachments.TitleDocumentAttachment
import org.slf4j.Logger
import org.slf4j.LoggerFactory

class Metadata(val dctOpt: Option[DCT], val idOpt: Option[String],
  val titleOpt: Option[String], val locationOpt: Option[String]) {

  def attachToDoc(doc: Document): Unit = {
    doc.id = idOpt
    dctOpt.foreach { dct =>
      DctDocumentAttachment.setDct(doc, dct)
      doc.setDCT(dct.text)
    }
    titleOpt.foreach { title =>
      TitleDocumentAttachment.setTitle(doc, title)
    }
    locationOpt.foreach { location =>
      LocationDocumentAttachment.setLocation(doc, location)
    }
  }
}

object Metadata {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  protected def newDct(timeNormFinderOpt: Option[TimeNormFinder], dctStringOpt: Option[String]): Option[DCT] = {
    val dctOpt = for (dctString <- dctStringOpt; timeNormFinder <- timeNormFinderOpt) yield {
      val dctOpt = timeNormFinder.parseDctString(dctString)
      if (dctOpt.isEmpty)
        logger.warn(s"""The document creation time, "$dctString", could not be parsed.  Proceeding without...""")
      dctOpt
    }
    dctOpt.flatten
  }

  def apply(): Metadata = {
    new Metadata(None, None, None, None)
  }

  def apply(timeNormFinderOpt: Option[TimeNormFinder], dctStringOpt: Option[String], idOpt: Option[String]): Metadata = {
    val dctOpt = newDct(timeNormFinderOpt, dctStringOpt)

    new Metadata(dctOpt, idOpt, None, None)
  }

  def apply(dctOpt: Option[DCT] = None, idOpt: Option[String] = None,
      titleOpt: Option[String] = None, locationOpt: Option[String] = None): Metadata = {
    new Metadata(dctOpt, idOpt, titleOpt, locationOpt)
  }
}
