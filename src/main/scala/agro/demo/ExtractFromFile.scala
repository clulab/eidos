package agro.demo



import java.io.{File, FilenameFilter, PrintWriter}

import org.clulab.odin.Mention
import org.clulab.processors.Document
import org.clulab.wm.AgroSystem
import utils.DisplayUtils.printMentions

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object ExtractFromFile  extends App {

  val agroSystem = new AgroSystem()
  val files = findFiles("/Users/bsharp/github/research/wmseed/src/main/resources/org/clulab/wm/rapdocs", "txt")
  val outputDir = "/Users/bsharp"

  val pw = new PrintWriter(s"$outputDir/prettyRAPexample.output")

  for (filename <- files) {
    if (filename.getName.contains("Example")) {
      val source = scala.io.Source.fromFile(filename)
      val text = source.getLines().toArray.head
      val doc = agroSystem.proc.annotate(text)
      val docTexts = doc.sentences.map(_.getSentenceText())
      pw.println(s"Filename: ${filename.getName}\n\tText: ${docTexts.mkString(" ")}")
      for (sentenceText <- docTexts){
        pw.println(s"\nSENTENCE: ${sentenceText}")
        val sentenceDoc = agroSystem.proc.annotate(sentenceText)

        val mentions = agroSystem.extractFrom(sentenceDoc).distinct
        println (s"Number of mentions found: ${mentions.length}")
        printMentions(mentions, sentenceDoc, pw)
        prettyPrint(mentions, sentenceDoc, pw)
    }

    }

  }
  pw.close()


  def prettyPrint(mentions:Seq[Mention], doc:Document, pw: PrintWriter): Unit = {
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
      println("RAP Parameters:")
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

  def findFiles(collectionDir: String, extension: String): Seq[File] = {
    val dir = new File(collectionDir)
    dir.listFiles(new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name.endsWith(extension)
    })
  }

}
