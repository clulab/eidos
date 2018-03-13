package org.clulab.wm.eidos.utils

import java.io.PrintWriter
import org.clulab.odin._
import org.clulab.processors.{Document, Sentence}
import scala.runtime.ZippedTraversable3.zippedTraversable3ToTraversable

object DisplayUtils {


  def mentionsToDisplayString(
    mentions: Seq[Mention],
    doc: Document,
    printDeps: Boolean = false,
    newline: String = "\n",
    tab: String = "\t"): String = {

    val sb = new StringBuffer()
    val mentionsBySentence = mentions groupBy (_.sentence) mapValues (_.sortBy(_.start)) withDefaultValue Nil
    for ((s, i) <- doc.sentences.zipWithIndex) {
      sb.append(s"sentence #$i $newline")
      sb.append(s.getSentenceText + newline)
      sb.append("Tokens: " + (s.words.indices, s.words, s.tags.get).zipped.mkString(", ") + newline)
      if (printDeps) sb.append(syntacticDependenciesToString(s) + newline)
      sb.append(newline)


      val sortedMentions = mentionsBySentence(i).sortBy(_.label)
      val (events, entities) = sortedMentions.partition(_ matches "Event")
      val (tbs, rels) = entities.partition(_.isInstanceOf[TextBoundMention])
      val sortedEntities = tbs ++ rels.sortBy(_.label)
      sb.append(s"entities: $newline")
      sortedEntities.foreach(e => sb.append(s"${mentionToDisplayString(e, newline, tab)} $newline"))

      sb.append(newline)
      sb.append(s"events: $newline")
      events.foreach(e => sb.append(s"${mentionToDisplayString(e, newline, tab)} $newline"))
      sb.append(s"${"=" * 50} $newline")
    }
    sb.toString()
  }

  def mentionToDisplayString(mention: Mention, newline: String, tab: String): String = {
    val sb = new StringBuffer()
    val boundary = s"$tab ${"-" * 30} $newline"
    sb.append(s"${mention.labels} => ${mention.text} $newline")
    sb.append(boundary)
    sb.append(s"$tab Rule => ${mention.foundBy} $newline")
    val mentionType = mention.getClass.toString.split("""\.""").last
    sb.append(s"$tab Type => $mentionType $newline")
    sb.append(boundary)
    mention match {
      case tb: TextBoundMention =>
        sb.append(s"$tab ${tb.labels.mkString(", ")} => ${tb.text} $newline")
        if (tb.attachments.nonEmpty) sb.append(s"$tab  * Attachments: ${attachmentsString(tb.attachments)} $newline")
      case em: EventMention =>
        sb.append(s"$tab trigger => ${em.trigger.text} $newline")
        if (em.trigger.attachments.nonEmpty) sb.append(s"$tab  * Attachments: ${attachmentsString(em.trigger.attachments)} $newline")
        sb.append(argumentsToString(em, newline, tab) + newline)
        if (em.attachments.nonEmpty) {
          sb.append(s"$tab Event Attachments: ${attachmentsString(em.attachments)} $newline")
        }
      case rel: RelationMention =>
        sb.append(argumentsToString(rel, newline, tab) + newline)
        if (rel.attachments.nonEmpty) {
          sb.append(s"$tab Relation Attachments: ${attachmentsString(rel.attachments)} $newline")
        }
      case _ => ()
    }
    sb.append(s"$boundary $newline")
    sb.toString()
  }

  def argumentsToString(b: Mention, newline: String, tab: String): String = {
    val sb = new StringBuffer
    b.arguments foreach {
      case (argName, ms) =>
        ms foreach { v =>
          sb.append(s"$tab $argName ${v.labels.mkString("(", ", ", ")")} => ${v.text} $newline")
          if (v.attachments.nonEmpty) sb.append(s"$tab  * Attachments: ${attachmentsString(v.attachments)} $newline")
        }
    }
    sb.toString()
  }

  def attachmentsString(mods: Set[Attachment]): String = s"${mods.mkString(", ")}"


  def displayMentions(mentions: Seq[Mention], doc: Document, printDeps: Boolean = false): Unit = {
    println(mentionsToDisplayString(mentions, doc, printDeps, "\n", "\t"))
  }

  def printMentions(mentions: Seq[Mention], doc: Document, pw: PrintWriter, printDeps: Boolean = false): Unit = {
    pw.println(mentionsToDisplayString(mentions, doc, printDeps, "\n", "\t"))
  }

  def syntacticDependenciesToString(s:Sentence): String = {
    if (s.dependencies.isDefined) {
      s.dependencies.get.toString
    } else "[Dependencies not defined]"
  }


  def displayMention(mention: Mention) = println(mentionToDisplayString(mention, "\n", "\t"))


  def webAppMention(mention: Mention): String = {
    mentionToDisplayString(mention, "<br>", htmltab)

//    val sb = new StringBuilder
//    val boundary = s"${tab}${"-" * 30}<br>"
//    sb.append(s"${mention.labels} => ${mention.text}<br>")
//    sb.append(boundary)
//    sb.append(s"${tab}Rule => ${mention.foundBy}<br>")
//    val mentionType = mention.getClass.toString.split("""\.""").last
//    sb.append(s"${tab}Type => $mentionType<br>")
//    sb.append(boundary)
//    mention match {
//      case tb: TextBoundMention =>
//        sb.append(s"${tab}${tb.labels.mkString(", ")} => ${tb.text}<br>")
//        if (tb.attachments.nonEmpty) sb.append(s"${tab}  * Attachments: ${attachmentsString(tb.attachments)}<br>")
//      case em: EventMention =>
//        sb.append(s"${tab}trigger => ${em.trigger.text}<br>")
//        if (em.trigger.attachments.nonEmpty) sb.append(s"${tab}  * Attachments: ${attachmentsString(em.trigger.attachments)}<br>")
//        sb.append(webAppArguments(em))
//        if (em.attachments.nonEmpty) {
//          sb.append(s"${tab}Event Attachments: ${attachmentsString(em.attachments)}<br>")
//        }
//      case rel: RelationMention =>
//        sb.append(webAppArguments(rel))
//        if (rel.attachments.nonEmpty) {
//          sb.append(s"${tab}Relation Attachments: ${attachmentsString(rel.attachments)}<br>")
//        }
//      case _ => ()
//    }
//    sb.append(s"$boundary<br>")
//
//    sb.toString()
  }

  def printMention(mention: Mention, pw: PrintWriter) = pw.println(mentionToDisplayString(mention, "\n", "\t"))

//  def webAppArguments(b: Mention): String = {
//    val sb = new StringBuilder
//    b.arguments foreach {
//      case (argName, ms) =>
//        ms foreach { v =>
//          sb.append(s"${tab}$argName ${v.labels.mkString("(", ", ", ")")} => ${v.text}<br>")
//          if (v.attachments.nonEmpty) sb.append(s"${tab}  * Attachments: ${attachmentsString(v.attachments)}<br>")
//        }
//    }
//    sb.toString()
//  }


  // html tab
  val htmltab:String = "&nbsp;&nbsp;&nbsp;&nbsp;"
}
