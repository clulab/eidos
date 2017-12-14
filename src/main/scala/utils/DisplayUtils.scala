package utils

import java.io.PrintWriter

import org.clulab.odin._
import org.clulab.processors.{Document, Sentence}

object DisplayUtils {


  def displayMentions(mentions: Seq[Mention], doc: Document): Unit = {
    val mentionsBySentence = mentions groupBy (_.sentence) mapValues (_.sortBy(_.start)) withDefaultValue Nil
    for ((s, i) <- doc.sentences.zipWithIndex) {
      println(s"sentence #$i")
      println(s.getSentenceText)
      println("Tokens: " + (s.words.indices, s.words, s.tags.get).zipped.mkString(", "))
//      printSyntacticDependencies(s)
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
        println(s"\ttrigger => ${em.trigger.text}}")
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
      case _ => ()
    }
    println(s"$boundary\n")
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
      case em: EventMention =>
        pw.println(s"\ttrigger => ${em.trigger.text}")
        printArguments(em, pw)
      case rel: RelationMention =>
        printArguments(rel, pw)
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

  def printArguments(b: Mention, pw: PrintWriter): Unit = {
    b.arguments foreach {
      case (argName, ms) =>
        ms foreach { v =>
          pw.println(s"\t$argName ${v.labels.mkString("(", ", ", ")")} => ${v.text}")
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
}
