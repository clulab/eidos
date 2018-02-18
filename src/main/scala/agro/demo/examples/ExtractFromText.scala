package agro.demo.examples

import org.clulab.wm.EidosSystem
import utils.DisplayUtils.displayMention

object ExtractFromText extends App {

  val text = "Water trucking has decreased due to the cost of fuel."

  // Initialize the reader
  val reader = new EidosSystem()

  // Extract the mentions
  val mentions = reader.extractFrom(text)

  // Display
  mentions.foreach(displayMention)
}
