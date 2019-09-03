package org.clulab.wm.eidos.apps

import java.io.PrintWriter

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import org.clulab.odin.Mention
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.utils.DisplayUtils.printMention
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils

object ExtractFromFile extends App {
  val inputDir = args(0)
  val outputFile = args(1)
  val files = FileUtils.findFiles(inputDir, "txt")
  println(s"There are ${files.length} files...")

  (FileUtils.printWriterFromFile(s"$outputFile")).autoClose { pw =>
    val ieSystem = new EidosSystem()

    for (filename <- files) {
      val text = FileUtils.getTextFromFile(filename)
      println(s"There are ${text.split('\n').length} lines in the file...")
      val annotatedDoc = ieSystem.extractFromText(text)
      val doc = annotatedDoc.document
      pw.println(s"Filename: ${filename.getName}")

      // keep the EidosMentions that are relevant to the CAG
      val cagEdgeMentions = annotatedDoc.odinMentions.filter(m => EidosSystem.CAG_EDGES.contains(m.label))
      val cagEdgeArguments = cagEdgeMentions.flatMap(mention => mention.arguments.values.flatten.toSeq)
      val eidosMentions = annotatedDoc.eidosMentions.filter(em => ieSystem.components.stopwordManager.isCAGRelevant(em.odinMention, cagEdgeMentions, cagEdgeArguments))

      val mentionsBySentence = eidosMentions.groupBy(_.odinMention.sentence).toSeq.sortBy(_._1)
      for ((sentence, sentenceMentions) <- mentionsBySentence) {
        pw.println(s"\nSENTENCE ${sentence}: ${doc.sentences(sentence).getSentenceText}")
        println(s"Number of Eidos mentions found: ${sentenceMentions.length}")
        sentenceMentions.foreach(
          m => {
            pw.println(s"CanonicalName: ${m.canonicalName}")
            pw.println(s"OntologyGrounding: \n\t${m.grounding.values.mkString("\n\t")}")
            printMention(m.odinMention, pw)

          }
        )
        pw.println(s"${"=" * 100}")
      }
    }
  }

  def prettyPrint(mentions:Seq[Mention], pw: PrintWriter): Unit = {
    val events = mentions.filter(_ matches "Event")
    val params = new mutable.HashMap[String, ListBuffer[(String, String, String)]]()
    for(e <- events) {
      val f = formal(e)
      if(f.isDefined) {
        val just = e.text
        val sent = e.sentenceObj.getSentenceText
        val quantifier = e.arguments.get("quantifier") match {
          case Some(quantifierMentions) => quantifierMentions.map(_.text).head
          case None => "None"
        }
        params.getOrElseUpdate(f.get, new ListBuffer[(String, String, String)]) += new Tuple3(just, sent, quantifier)
      }
    }

    if(params.nonEmpty) {
      println("Eidos Parameters:")
      for (k <- params.keySet) {
        val evidence = params.get(k).get
        pw.println(s"$k: ${evidence.size} instances:")
        for (e <- evidence) {
          pw.println(s"\tJustification: [${e._1}]")
          pw.println(s"""\tSentence: "${e._2}"""")
          pw.println(s"\tQuantifier: ${e._3}")
        }
        pw.println()
      }
    }
  }

  def formal(e: Mention): Option[String] = {
    val t =
        if (e matches "Decrease") Some("DECREASE")
        else if (e matches "Increase") Some("INCREASE")
        else None

    t.map(t => s"$t of ${e.arguments.get("theme").get.head.label}")
  }
}
