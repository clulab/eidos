package org.clulab.wm.eidoscommon

import org.clulab.processors.Sentence
import org.clulab.struct.Interval

class Canonicalizer(stopwordManaging: StopwordManaging, tagSet: TagSet) {

  // Here we use the lemma because the stopwords etc are written against them
  def isCanonicalLemma(lemma: String, tag: String, ner: String): Boolean =
    tagSet.isOntologyContent(tag) &&
        !stopwordManaging.containsStopwordStrict(lemma) &&
        !EidosParameters.STOP_NER.contains(ner)

  def canonicalWordsFromSentence(s: Sentence, tokenInterval: Interval, excludedWords: Set[String] = Set()): Seq[String] = {
    val words = s.words
    val lemmas = s.lemmas.get
    val tags = s.tags.get
    val ners = s.entities.get
    // Here we use words because the embeddings are expecting words
    val contentWords = for {
      i <- tokenInterval.start until tokenInterval.end
      if isCanonicalLemma(lemmas(i), tags(i), ners(i))
      if !excludedWords.contains(words(i))
    } yield words(i)

    if (contentWords.isEmpty)
      words.slice(tokenInterval.start, tokenInterval.end)   // fixme -- better and cleaner backoff
    else
      contentWords
  }
}
