package org.clulab.wm.eidos.utils

import org.clulab.processors.corenlp.CoreNLPDocument
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.processors.{Document, Processor, Sentence}

trait DocumentFilter {
  def filter(doc: Document): Document
}

/**
  * Filter to remove sentences from the Document based on length, as determined by the number of word tokens.
  * Sentences which are too long are not trimmed, they are removed (under presumption that they are
  * likely to be garbage/noise.
  * 
  * @param cutoff the max number of words (exclusive) allowed in a sentence, default = 200
  */
class FilterByLength(processor: Processor, cutoff: Int = 200) extends DocumentFilter {

  def filter(doc: Document): Document = {
    // Iterate through the sentences, any sentence that is too long (number of tokens), remove
    val kept = doc.sentences.filter(s => s.words.length < cutoff)
    val sentenceStrings = kept.map(_.getSentenceText)
    println(s"keeping ${kept.length} sentences")
    // Return a new document from these sentences
    processor.mkDocumentFromSentences(sentenceStrings, keepText = true)
  }
}
object FilterByLength {
  def apply(processor: Processor, cutoff: Int = 200): FilterByLength = new FilterByLength(processor, cutoff)
}

