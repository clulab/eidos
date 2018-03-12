package org.clulab.wm.eidos.utils

import java.io.PrintWriter
import org.clulab.odin._
import org.clulab.processors.{Document, Sentence}
import scala.runtime.ZippedTraversable3.zippedTraversable3ToTraversable

object DisplayUtils {


  def displayMentions(mentions: Seq[Mention], doc: Document, printDeps: Boolean = false): Unit = {
    val mentionsBySentence = mentions groupBy (_.sentence) mapValues (_.sortBy(_.start)) withDefaultValue Nil
    for ((s, i) <- doc.sentences.zipWithIndex) {
      println(s"sentence #$i")
      println(s.getSentenceText)
      println("Tokens: " + (s.words.indices, s.words, s.tags.get).zipped.mkString(", "))
      if(printDeps){
          printSyntacticDependencies(s)
      }
      println

      val sortedMentions = mentionsBySentence(i).sortBy(_.label)
      val (events, entities) = sortedMentions.partition(_ matches "Event")
      val (tbs, rels) = entities.partition(_.isInstanceOf[TextBoundMention])
      val sortedEntities = tbs ++ rels.sortBy(_.label)
      println("entities:")
      sortedEntities foreach displayMention

      println
      println("events:")
      events foreach displayMention
      println("=" * 50)
    }
  }

  def printMentions(mentions: Seq[Mention], doc: Document, pw: PrintWriter): Unit = {
    val mentionsBySentence = mentions groupBy (_.sentence) mapValues (_.sortBy(_.start)) withDefaultValue Nil
    for ((s, i) <- doc.sentences.zipWithIndex) {
      pw.println(s"sentence #$i")
      pw.println(s.getSentenceText)
      pw.println("Tokens: " + (s.words.indices, s.words, s.tags.get).zipped.mkString(", "))
      //      printSyntacticDependencies(s)
      pw.println

      val sortedMentions = mentionsBySentence(i).sortBy(_.label)
      val (events, entities) = sortedMentions.partition(_ matches "Event")
      val (tbs, rels) = entities.partition(_.isInstanceOf[TextBoundMention])
      val sortedEntities = tbs ++ rels.sortBy(_.label)
      pw.println("entities:")
      sortedEntities.foreach(e => printMention(e, pw))

      pw.println
      pw.println("events:")
      events.foreach(e => printMention(e, pw))
      pw.println("=" * 50)
    }
  }

  def printSyntacticDependencies(s:Sentence): Unit = {
    if(s.dependencies.isDefined) {
      println(s.dependencies.get.toString)
    }
  }

  def attachmentsString(mods: Set[Attachment]): String = s"${mods.mkString(", ")}"

  def displayMention(mention: Mention) {
    val boundary = s"\t${"-" * 30}"
    println(s"${mention.labels} => ${mention.text}")
    println(boundary)
    println(s"\tRule => ${mention.foundBy}")
    val mentionType = mention.getClass.toString.split("""\.""").last
    println(s"\tType => $mentionType")
    println(boundary)
    mention match {
      case tb: TextBoundMention =>
        println(s"\t${tb.labels.mkString(", ")} => ${tb.text}")
        if (tb.attachments.nonEmpty) println(s"\t  * Attachments: ${attachmentsString(tb.attachments)}")
      case em: EventMention =>
        println(s"\ttrigger => ${em.trigger.text}")
        if (em.trigger.attachments.nonEmpty) println(s"\t  * Attachments: ${attachmentsString(em.trigger.attachments)}")
        displayArguments(em)
        if (em.attachments.nonEmpty) {
          println(s"\tEvent Attachments: ${attachmentsString(em.attachments)}")
        }
      case rel: RelationMention =>
        displayArguments(rel)
        if (rel.attachments.nonEmpty) {
          println(s"\tRelation Attachments: ${attachmentsString(rel.attachments)}")
        }
      case cs: CrossSentenceMention =>
        displayArguments(cs)
        if (cs.attachments.nonEmpty) {
          println(s"\tCross-sentence Attachments: ${attachmentsString(cs.attachments)}")
        }
      case _ => ()
    }
    println(s"$boundary\n")
  }

  def webAppMention(mention: Mention): String = {
    val sb = new StringBuilder
    val boundary = s"${tab}${"-" * 30}<br>"
    sb.append(s"${mention.labels} => ${mention.text}<br>")
    sb.append(boundary)
    sb.append(s"${tab}Rule => ${mention.foundBy}<br>")
    val mentionType = mention.getClass.toString.split("""\.""").last
    sb.append(s"${tab}Type => $mentionType<br>")
    sb.append(boundary)
    mention match {
      case tb: TextBoundMention =>
        sb.append(s"${tab}${tb.labels.mkString(", ")} => ${tb.text}<br>")
        if (tb.attachments.nonEmpty) sb.append(s"${tab}  * Attachments: ${attachmentsString(tb.attachments)}<br>")
      case em: EventMention =>
        sb.append(s"${tab}trigger => ${em.trigger.text}<br>")
        if (em.trigger.attachments.nonEmpty) sb.append(s"${tab}  * Attachments: ${attachmentsString(em.trigger.attachments)}<br>")
        sb.append(webAppArguments(em))
        if (em.attachments.nonEmpty) {
          sb.append(s"${tab}Event Attachments: ${attachmentsString(em.attachments)}<br>")
        }
      case rel: RelationMention =>
        sb.append(webAppArguments(rel))
        if (rel.attachments.nonEmpty) {
          sb.append(s"${tab}Relation Attachments: ${attachmentsString(rel.attachments)}<br>")
        }
      case _ => ()
    }
    sb.append(s"$boundary<br>")

    sb.toString()
  }

  def printMention(mention: Mention, pw: PrintWriter) {
    val boundary = s"\t${"-" * 30}"
    pw.println(s"${mention.labels} => ${mention.text}")
    pw.println(boundary)
    pw.println(s"\tRule => ${mention.foundBy}")
    val mentionType = mention.getClass.toString.split("""\.""").last
    pw.println(s"\tType => $mentionType")
    pw.println(boundary)
    mention match {
      case tb: TextBoundMention =>
        pw.println(s"\t${tb.labels.mkString(", ")} => ${tb.text}")
        if (tb.attachments.nonEmpty) pw.println(s"\t  * Attachments: ${attachmentsString(tb.attachments)}")
      case em: EventMention =>
        pw.println(s"\ttrigger => ${em.trigger.text}")
        if (em.trigger.attachments.nonEmpty) pw.println(s"\t  * Attachments: ${attachmentsString(em.trigger.attachments)}")
        printArguments(em, pw)
        if (em.attachments.nonEmpty) {
          pw.println(s"\tEvent Attachments: ${attachmentsString(em.attachments)}")
        }
      case rel: RelationMention =>
        printArguments(rel, pw)
        if (rel.attachments.nonEmpty) {
          pw.println(s"\tRelation Attachments: ${attachmentsString(rel.attachments)}")
        }
      case _ => ()
    }
    pw.println(s"$boundary\n")
  }



  def displayArguments(b: Mention): Unit = {
    b.arguments foreach {
      case (argName, ms) =>
        ms foreach { v =>
          println(s"\t$argName ${v.labels.mkString("(", ", ", ")")} => ${v.text}")
          if (v.attachments.nonEmpty) println(s"\t  * Attachments: ${attachmentsString(v.attachments)}")
        }
    }
  }

  def webAppArguments(b: Mention): String = {
    val sb = new StringBuilder
    b.arguments foreach {
      case (argName, ms) =>
        ms foreach { v =>
          sb.append(s"${tab}$argName ${v.labels.mkString("(", ", ", ")")} => ${v.text}<br>")
          if (v.attachments.nonEmpty) sb.append(s"${tab}  * Attachments: ${attachmentsString(v.attachments)}<br>")
        }
    }
    sb.toString()
  }

  def printArguments(b: Mention, pw: PrintWriter): Unit = {
    b.arguments foreach {
      case (argName, ms) =>
        ms foreach { v =>
          pw.println(s"\t$argName ${v.labels.mkString("(", ", ", ")")} => ${v.text}")
          if (v.attachments.nonEmpty) pw.println(s"\t  * Attachments: ${attachmentsString(v.attachments)}")
        }
    }
  }

  def argumentsToString(b: Mention): String = {
    val sb = new StringBuffer
    b.arguments foreach {
      case (argName, ms) =>
        ms foreach { v =>
          sb.append(s"\t$argName ${v.labels.mkString("(", ", ", ")")} => ${v.text}")
        }
    }
    sb.toString
  }

  // html tab
  def tab():String = "&nbsp;&nbsp;&nbsp;&nbsp;"
}
