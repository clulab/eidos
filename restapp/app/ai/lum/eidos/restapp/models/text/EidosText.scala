package ai.lum.eidos.restapp.models.text

import org.clulab.wm.eidos.context.DCT

import org.json4s.JValue

trait EidosText {
  def getText: String
  def getTitleOpt: Option[String]
  def getIdOpt: Option[String]
  def getDctOpt: Option[DCT]
  def getLocationOpt: Option[String]
  def toJson: JValue
}
