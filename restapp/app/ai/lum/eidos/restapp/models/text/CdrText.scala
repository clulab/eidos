package ai.lum.eidos.restapp.models.text

import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.utils.meta.DartZipMetaUtils

import org.json4s.DefaultFormats
import org.json4s.JValue
import org.json4s.JsonDSL._

class CdrText(cdr: JValue) extends EidosText {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  protected val text: String = (cdr \ "extracted_text").extract[String]
  protected val titleOpt: Option[String] = DartZipMetaUtils.getDartDocumentTitle(Some(cdr))
  protected val idOpt: Option[String] = DartZipMetaUtils.getDartDocumentId(Some(cdr))
  // It would be nice if this threw an exception on bad data.
  // Check whether keys are there at all and throw exception if parsing returns None.
  protected val dctOpt: Option[DCT] = DartZipMetaUtils.getDocumentCreationTime(Some(cdr))
  protected val locationOpt: Option[String] = DartZipMetaUtils.getDartDocumentLocation(Some(cdr))

  override def getText: String = text

  override def getTitleOpt: Option[String] = titleOpt

  override def getIdOpt: Option[String] = idOpt

  override def getDctOpt: Option[DCT] = dctOpt

  override def getLocationOpt: Option[String] = locationOpt

  override def toString: String =
    s"""
       | text = $text
       | title = $titleOpt
       | id = $idOpt
       | dct = ${dctOpt.map(_.interval.start.toString)}
       | location = $locationOpt
       |""".stripMargin

  def toJson: JValue = {
    ("text" -> text) ~
    ("title" -> titleOpt) ~
    ("id" -> idOpt) ~
    ("dct" -> dctOpt.map(_.interval.start.toString)) ~
    ("location" -> locationOpt)
  }
}
