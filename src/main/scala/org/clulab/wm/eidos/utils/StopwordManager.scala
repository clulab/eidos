package org.clulab.wm.eidos.utils

import org.clulab.odin.Mention

trait StopwordManaging {
  def containsStopword(stopword: String): Boolean
}

class StopwordManager(stopwordsPath: String, transparentPath: String) extends StopwordManaging {
  protected def stopwords = FileUtils.getCommentedTextsFromResource(stopwordsPath).toSet
  protected def transparentWords = FileUtils.getCommentedTextsFromResource(transparentPath).toSet

  protected val bothWords = stopwords ++ transparentWords

  def containsStopword(stopword: String): Boolean = bothWords.contains(stopword)

  protected def hasContent(mention: Mention): Boolean = {
    val lemmas = mention.lemmas.get
    val tags = mention.tags.get
    val entities = mention.entities.get

    // println(s"Checking mention: ${m.text}")
    lemmas.indices.exists { i =>
      !containsStopword(lemmas(i)) &&
        !StopwordManager.STOP_POS.contains(tags(i)) &&
        !StopwordManager.STOP_NER.contains(entities(i))
    }
  }

  def filterStopTransparent(mentions: Seq[Mention]): Seq[Mention] =
      // Remove mentions which are entirely stop/transparent words
      mentions.filter(hasContent)
}

object StopwordManager {
  val STOP_POS: Set[String] = Set("CD")
  val STOP_NER: Set[String] = Set("DATE", "DURATION", "LOCATION", "MONEY", "NUMBER", "ORDINAL", "ORGANIZATION", "PERCENT", "PERSON", "PLACE", "SET", "TIME")

  def apply(stopwordsPath: String, transparentPath: String) = new StopwordManager(stopwordsPath, transparentPath)
}