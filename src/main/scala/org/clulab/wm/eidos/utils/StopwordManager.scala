package org.clulab.wm.eidos.utils

import org.clulab.odin._
import org.clulab.wm.eidos.{EidosActions, EidosSystem}

trait StopwordManaging {
  def containsStopword(stopword: String): Boolean
  def containsStopwordStrict(stopword: String): Boolean = containsStopword(stopword)
}

class StopwordManager(stopwordsPath: String, transparentPath: String) extends StopwordManaging {
  protected def stopwords: Set[String] = FileUtils.getCommentedTextSetFromResource(stopwordsPath)
  protected def transparentWords: Set[String] = FileUtils.getCommentedTextSetFromResource(transparentPath)

  protected val bothWords = stopwords ++ transparentWords

  def containsStopword(stopword: String): Boolean = bothWords.contains(stopword)

  def hasContent(mention: Mention, state: State): Boolean = hasContent(mention) || resolvedCoref(mention, state)

  def hasContent(mention: Mention): Boolean = {
    val lemmas = mention.lemmas.get
    val tags = mention.tags.get
    val entities = mention.entities.get

    //println(s"Checking mention: ${mention.text}")
    lemmas.indices.exists { i =>
      isContentPOS(tags(i)) &&
      tags(i) != "VBN" && // we don't want entities/concepts which consist ONLY of a VBN
      !containsStopword(lemmas(i)) &&
        !StopwordManager.STOP_POS.contains(tags(i)) &&
        !StopwordManager.STOP_NER.contains(entities(i))
    }
  }

  def resolvedCoref(mention: Mention, state: State): Boolean = {
    if (hasCorefToResolve(mention)) {
      val corefRelations = state.allMentions.filter(m => m.matches(EidosSystem.COREF_LABEL))
      corefRelations.exists(cr => cr.arguments.values.toSeq.flatten.contains(mention))
    }
    else false
  }

  def hasCorefToResolve(m: Mention): Boolean = {
    m match {
      case tb: TextBoundMention => EidosActions.startsWithCorefDeterminer(tb)
      case rm: RelationMention => EidosActions.existsDeterminerCause(rm)
      case em: EventMention => EidosActions.existsDeterminerCause(em)
      case _ => false
    }
  }

  def isContentPOS(tag: String): Boolean = StopwordManager.CONTENT_POS_PREFIXES.exists(prefix => tag.startsWith(prefix))


  def filterStopTransparent(mentions: Seq[Mention]): Seq[Mention] =
      // Remove mentions which are entirely stop/transparent words
      mentions.filter(hasContent)

  override def containsStopwordStrict(stopword: String): Boolean = stopwords.contains(stopword)

}

object StopwordManager {
  val CONTENT_POS_PREFIXES: Set[String] = Set("ADJ", "NOUN", "NN", "PROPN", "VERB", "VB", "JJ")
  val STOP_POS: Set[String] = Set("CD")
  val STOP_NER: Set[String] = Set("DATE", "DURATION", "LOCATION", "MONEY", "NUMBER", "ORDINAL", "ORGANIZATION", "PERCENT", "PERSON", "PLACE", "SET", "TIME")

  def apply(stopwordsPath: String, transparentPath: String) = new StopwordManager(stopwordsPath, transparentPath)
}