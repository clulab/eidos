package org.clulab.wm.eidoscommon

import org.clulab.processors.Sentence
import org.clulab.struct.Interval

class Canonicalizer(stopwordManaging: StopwordManaging, tagSet: TagSet) {

  // Here we use the lemma because the stopwords etc are written against them
  def isCanonicalLemma(lemma: String, tag: String, ner: String): Boolean =
    tagSet.isOntologyContent(tag) &&
        !stopwordManaging.containsStopwordStrict(lemma) &&
        !stopwordManaging.containsStopwordNer(ner)

  def canonicalWordsFromSentence(s: Sentence, tokenInterval: Interval, excludedWords: Set[String] = Set()): Seq[String] = {
    val canonicalWords = canonicalWordsFromSentenceAt(s, tokenInterval, excludedWords)

    if (canonicalWords.nonEmpty) canonicalWords
    else tokenInterval.map(s.words) // fixme -- better and cleaner backoff
  }

  def canonicalWordsFromSentenceAt(s: Sentence, tokenIndexes: Seq[Int], excludedWords: Set[String] = Set()): Seq[String] = {
    val words = s.words
    val lemmas = s.lemmas.get
    val tags = s.tags.get
    val ners = s.entities.get
    // Here we use words because the embeddings are expecting words
    val contentWords = for {
      i <- tokenIndexes
      if isCanonicalLemma(lemmas(i), tags(i), ners(i))
      word = words(i)
      if !excludedWords.contains(word)
    } yield word

    contentWords
  }
}
