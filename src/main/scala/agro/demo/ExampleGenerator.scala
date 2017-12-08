package agro.demo

import java.io.PrintWriter

import org.json4s.jackson.JsonMethods._
import org.clulab.wm.AgroSystem
import org.clulab.wm.serialization.json.WMJSONSerializer
import utils.DisplayUtils.displayMentions


object ExampleGenerator extends App {

  // Needed for json4s
  implicit val formats = org.json4s.DefaultFormats


  // creates an extractor engine using the rules and the default actions
  val ieSystem = new AgroSystem()
  val proc = ieSystem.proc

  val text = "The government promotes improved cultivar to boost agricultural production for ensuring food security. However, the policy to seriously cut down the use of inorganic fertilizer and phase out the fertilizer subsidy results in deteriorating biophysical conditions, less use of inorganic fertilizer, less water, significantly reduced farm sizes which lead to low benefit from the improved cultivar."
  val doc = proc.annotate(text)

  // extract mentions from annotated document
  val mentions = ieSystem.extractFrom(doc).sortBy(m => (m.sentence, m.getClass.getSimpleName))

  // debug display the mentions
  displayMentions(mentions, doc)

  // serialize the mentions to a json file
  val mentionsJSON = WMJSONSerializer.jsonAST(mentions)
  println(pretty(render(mentionsJSON)))


  val pw = new PrintWriter("/Users/bsharp/wmExampleJson_dec7.txt")
  pw.println(pretty(render(mentionsJSON)))
  pw.close()



  // How to reconstitute the mentions:
  val newMentions = WMJSONSerializer.toMentions(mentionsJSON)

  println("\n*****************************************")
  println("             LOADED MENTIONS")
  println("*****************************************\n")
  println(s"Number of loaded mentions: ${newMentions.length}\n")

  newMentions foreach utils.DisplayUtils.displayMention

}
