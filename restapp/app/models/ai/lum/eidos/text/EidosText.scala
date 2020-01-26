package ai.lum.eidos.text

import org.clulab.wm.eidos.context.DCT

trait EidosText {
  def getText: String
  def getTitleOpt: Option[String]
  def getIdOpt: Option[String]
  def getDctOpt: Option[DCT]
  def getLocationOpt: Option[String]
}
