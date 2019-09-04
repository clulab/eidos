package org.clulab.wm.eidos.apps

import org.clulab.odin.{Attachment, EventMention, Mention}
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments.{Decrease, Increase, Quantification}
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils

import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.forkjoin.ForkJoinPool
import scala.collection.parallel.ForkJoinTaskSupport

object MakeRuleTSVs extends App {

  collection.parallel.ForkJoinTasks.defaultForkJoinPool

  val reader = new EidosSystem()
  val inputDir = args(0)
  val outputDir = args(1)
  val nCores = 4
  val files = FileUtils.findFiles(inputDir, "txt").par
  files.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(nCores))

  val annotatedDocuments = for {
      file <- files //foreach { file =>
      text = FileUtils.getTextFromFile(file)
      annotatedDocument = reader.extractFromText(text)
    } yield annotatedDocument

  // Organize
  val mentions = annotatedDocuments.seq.flatMap(ad => ad.odinMentions)
  val keptMentions = reader.components.stopwordManager.keepCAGRelevant(mentions)
  // create a map where the key is the Set of rule/rule components and the values are the Mentions
  val byRules = keptMentions.groupBy(_.foundBy)
    .map(grouping => (grouping._1.split("\\+\\+").toSet, grouping._2))

  // All the rules
  val masterSetRules = byRules.unzip._1.flatten.toSet

  val byRulesSet = masterSetRules.map(
    rule => ( rule, byRules
      // get the mentions that were foundBy this rule
      .filter(grp => grp._1.contains(rule))
      // flatten and keep only the mentions
      .flatMap(grpleft => grpleft._2)
    )
  )

  val header = Seq("Rule", "Cause", "-->", "Effect", "Trigger", "SentenceText").mkString("\t")

  // ok print them
  for ((rule, mentionsForRule) <- byRulesSet) {
    if (rule.contains(EidosSystem.CAUSAL_LABEL)) {
      (FileUtils.printWriterFromFile(s"${outputDir}/${rule}.tsv")).autoClose { pw =>
        pw.println(header)
        //      println(s"MENTIONS for RULE: ${rule}")
        for (m <- mentionsForRule) {
          val sentenceText = m.document.sentences(m.sentence).getSentenceText
          val toPrint = new ArrayBuffer[String]
          toPrint.append(rule)
          toPrint.appendAll(causalStringForCSV(m))
          toPrint.append(sentenceText)
          //        println(s"* Sentence: $sentenceText")
          //        displayMention(m)
          //        println(causalStringForCSV(m))
          pw.println(toPrint.mkString("\t"))
        }
      }
    }
  }

  def causalStringForCSV(m: Mention): Seq[String] = {
    val cm = m.asInstanceOf[EventMention]
    val cause = cm.arguments.get("cause")
    val effect = cm.arguments.get("effect")
    if (cause.nonEmpty && effect.nonEmpty) {
      val causes = cause.get.map(arg => argWithAttach(arg)).mkString(", ")
      val effects = effect.get.map(arg => argWithAttach(arg)).mkString(", ")
      Seq(causes, "-->", effects, s"trigger:${cm.trigger.text}")
    }
    else {
      throw new RuntimeException("Error: cause or effect args not defined")
    }
  }

  def argWithAttach(m: Mention): String = {
    val text = m.text
    val attachments = if (m.attachments.nonEmpty) s" +(${attachmentString(m.attachments)})" else ""


    s"$text${attachments}"
  }

  def attachmentString(as: Set[Attachment]): String = {
    as.map(a => attachmentString(a)).mkString(", ")
  }

  def attachmentString(a:Attachment): String =
    a match {
      case inc:Increase => s"INC-${inc.trigger}"
      case dec:Decrease => s"DEC-${dec.trigger}"
      case quant:Quantification => s"QUANT-${quant.trigger}"
      case _ => ""
    }

}
