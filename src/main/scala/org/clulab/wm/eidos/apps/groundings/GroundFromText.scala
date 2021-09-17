package org.clulab.wm.eidos.apps.groundings

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.grounders.EidosOntologyGrounder
import org.clulab.wm.eidos.utils.DisplayUtils.displayMention
import org.clulab.wm.eidos.utils.GroundingUtils

object GroundFromText extends App {

  val text = "Water trucking has decreased due to the cost of fuel."

  // Initialize the reader
  val reader = new EidosSystem()

  // Extract the mentions
  val annotatedDocument = reader.extractFromText(text)

  annotatedDocument.eidosMentions.foreach { eidosMention =>
    displayMention(eidosMention.odinMention)
    val stringOpt = GroundingUtils.getGroundingsStringOpt(eidosMention, EidosOntologyGrounder.PRIMARY_NAMESPACE)

    stringOpt.foreach { string =>
      val indent = "     "
      val indentedString = string.split('\n').mkString(indent, "\n" + indent, "")

      println(indentedString)
      println(indent + "------------------------------")
    }
  }
}
