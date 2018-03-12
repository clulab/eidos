package org.clulab.wm.eidos.apps

import java.io.PrintWriter

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import org.clulab.odin.Mention
import org.clulab.processors.Document
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.utils.DisplayUtils.printMention
import org.clulab.wm.eidos.utils.FileUtils.findFiles

object ExtractFromFile  extends App {

  val ieSystem = new EidosSystem()
  val files = findFiles("/Users/bsharp/WorldModelers/docNExample", "txt")
  println(s"There are ${files.length} files...")
  val outputDir = "/Users/bsharp/WorldModelers/docNExample"

  val pw = new PrintWriter(s"$outputDir/docN.output")

  for (filename <- files) {
    val source = scala.io.Source.fromFile(filename)
    val text = source.getLines().toArray.filter(line => !line.startsWith("#"))
    println(s"There are ${text.length} lines in the file...")
    val doc = ieSystem.proc.annotate(text.mkString(" "))
    pw.println(s"Filename: ${filename.getName}")
    val mentions = ieSystem.extractFrom(doc).distinct
    val keptMentions = ieSystem.keepCAGRelavant(mentions)
    val mentionsBySentence = keptMentions.groupBy(_.sentence).toSeq.sortBy(_._1)
    for ((sentence, sentenceMentions) <- mentionsBySentence){
      pw.println(s"\nSENTENCE ${sentence}: ${doc.sentences(sentence).getSentenceText()}")
      println (s"Number of mentions found: ${sentenceMentions.length}")
      sentenceMentions.foreach(m => printMention(m, pw))
      prettyPrint(mentions, pw)
    }

  }
  pw.close()


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
//          if (agroSystem.gradableAdjGroundingModel.contains(e._3)) {
//            val modelRow = agroSystem.gradableAdjGroundingModel(e._3)
//            pw.println(s"\t\t$modelRow")
//          }
        }
        pw.println()
      }
    }
  }

  def formal(e:Mention):Option[String] = {
    var t = ""
    if(e matches "Decrease") t = "DECREASE"
    else if(e matches "Increase") t = "INCREASE"
    else return None

    Some(s"$t of ${e.arguments.get("theme").get.head.label}")
  }

}
