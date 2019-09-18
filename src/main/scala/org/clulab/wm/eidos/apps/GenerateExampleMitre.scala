package org.clulab.wm.eidos.apps

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.DisplayUtils.{displayMention, displayMentions}
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.mentions.EidosTextBoundMention
import org.clulab.wm.eidos.utils.Canonicalizer
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils

import scala.collection.Seq

object GenerateExampleMitre extends App {

  // creates an extractor engine using the rules and the default actions
  val reader = new EidosSystem()
  val canonicalizer = new Canonicalizer(reader.components.stopwordManager)

  val text = "Particularly for people living in market-dependent urban areas, economic decline has caused a " +
    "significant reduction in access to staple food, clean water, and to a variety of foods. Conflict and economic decline " +
    "have led to violence and displacement. The rising cost of living and impact of the conflict have also " +
    "undermined people's ability to access safe water."

  // Extract causal mentions from the text
  val annotatedDocument = reader.extractFromText(text)

  // Display the groundings for all entities
  for (e <- annotatedDocument.eidosMentions.filter(_.odinMention matches "Entity")) {
    println(s"EidosMention: canonical=(${canonicalizer.canonicalize(e)})")
    displayMention(e.odinMention)
    println("Groundings:")
    e.asInstanceOf[EidosTextBoundMention].groundings.get.values.foreach(g => println(s"\t$g"))
  }

  // Default debug display of the mentions
  displayMentions(annotatedDocument.odinMentions, annotatedDocument.document)

  // Convert to JSON
  val corpus = new JLDCorpus(annotatedDocument)
  val mentionsJSONLD = corpus.serialize()

  // Write
  (FileUtils.printWriterFromFile("example_mar6.jsonld")).autoClose { pw =>
    pw.println(stringify(mentionsJSONLD, pretty = true))
  }
}
