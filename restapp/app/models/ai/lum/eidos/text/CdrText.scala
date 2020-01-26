package ai.lum.eidos.text

import org.clulab.wm.eidos.context.DCT

// Import JSON stuff
// Get this from different places like file as well

class CdrText(cdr: String) extends EidosText {
  protected val text = "hello"
  protected val idOpt: Option[String] = Some("")

  override def getText: String = ???

  override def getTitleOpt: Option[String] = ???

  override def getIdOpt: Option[String] = ???

  override def getDctOpt: Option[DCT] = ???

  override def getLocationOpt: Option[String] = ???
}
