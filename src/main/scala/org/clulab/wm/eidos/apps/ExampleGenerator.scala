package org.clulab.wm.eidos.apps

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.mentions.{EidosMention, EidosTextBoundMention}
import org.clulab.wm.eidos.serialization.json.WMJSONSerializer
import org.clulab.wm.eidos.utils.Canonicalizer
import org.clulab.wm.eidos.utils.DisplayUtils.{displayMention, displayMentions}
import org.json4s.jackson.JsonMethods._

object ExampleGenerator extends App {

  // Needed for json4s
  implicit val formats = org.json4s.DefaultFormats


  // creates an extractor engine using the rules and the default actions
  val ieSystem = new EidosSystem()

  val text = "The government uses significantly improved cultivar to boost agricultural production."
  val doc = ieSystem.annotate(text)

  // extract mentions from annotated document
  val mentions = ieSystem.extractFrom(doc).sortBy(m => (m.sentence, m.getClass.getSimpleName))
  val eidosMentions = EidosMention.asEidosMentions(mentions, new Canonicalizer(ieSystem), ieSystem)

  // Display the groundings for all entities
  for (e <- eidosMentions.filter(_.odinMention matches "Entity")) {
    println("EidosMention:")
    displayMention(e.odinMention)
    println("Groundings:")
    e.asInstanceOf[EidosTextBoundMention].grounding.values.foreach(g => println(s"\t$g"))
  }

  // Default debug display of the mentions
  displayMentions(mentions, doc)

  // serialize the mentions to a json file
  val mentionsJSON = WMJSONSerializer.jsonAST(mentions)
  println(pretty(render(mentionsJSON)))


//  val pw = FileUtils.newPrintWriterFromFile("/Users/bsharp/wmExampleJson_dec7.txt")
//  pw.println(pretty(render(mentionsJSON)))
//  pw.close()



  // How to reconstitute the mentions:
  val newMentions = WMJSONSerializer.toMentions(mentionsJSON)

  println("\n*****************************************")
  println("             LOADED MENTIONS")
  println("*****************************************\n")
  println(s"Number of loaded mentions: ${newMentions.length}\n")

  newMentions foreach org.clulab.wm.eidos.utils.DisplayUtils.displayMention

}
