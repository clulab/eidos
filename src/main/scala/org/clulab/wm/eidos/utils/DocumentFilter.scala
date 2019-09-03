package org.clulab.wm.eidos.utils

import org.clulab.processors.corenlp.CoreNLPDocument
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.processors.{Document, Processor}
import org.slf4j.{Logger, LoggerFactory}

trait DocumentFilter {
  def whileFiltered(document: Document)(transform: Document => Document): Document
}

class FilterByNothing extends DocumentFilter {

  def whileFiltered(doc: Document)(transform: Document => Document): Document = transform(doc)
}

object FilterByNothing {
  def apply() = new FilterByNothing
}

/**
  * Filter to remove sentences from the Document based on length, as determined by the number of word tokens.
  * Sentences which are too long are not trimmed, they are removed (under presumption that they are
  * likely to be garbage/noise.
  * 
  * @param cutoff the max number of words (exclusive) allowed in a sentence, default = 200
  */
class FilterByLength(processor: Processor, cutoff: Int = 200) extends DocumentFilter {

  def whileFiltered(doc: Document)(transform: Document => Document): Document = {
    val text = doc.text
    val filteredDoc = filter(doc)
    val transformedDoc = transform(filteredDoc)
    val unfilteredDoc = unfilter(transformedDoc, text)

    unfilteredDoc
  }

  protected def unfilter(doc: Document, textOpt: Option[String]): Document = {
    doc.text = textOpt
    doc
  }

  protected def filter(doc: Document): Document = {
    // Iterate through the sentences, any sentence that is too long (number of tokens), remove
    val sanitizedText = sanitizeText(doc)
    val kept = doc.sentences.filter(s => s.words.length < cutoff)
    val skipped = doc.sentences.length - kept.length
    val newDoc = Document(doc.id, kept, doc.coreferenceChains, doc.discourseTree, sanitizedText)
    val newerDoc = // This is a hack for lack of copy constructor for CoreNLPDocument
      if (doc.isInstanceOf[CoreNLPDocument])
        ShallowNLPProcessor.cluDocToCoreDoc(newDoc, keepText = true)
      else
        newDoc
    if (skipped != 0)
      FilterByLength.logger.info(s"skipping $skipped sentences")
    // Return a new document from these sentences
    newerDoc
  }

  protected def sanitizeText(doc: Document): Option[String] = doc.text.map { text =>
    // Assume that these characters are never parts of words.
    var newText = text.replace('\n', ' ').replace(0x0C.toChar, ' ')
    for (s <- doc.sentences if s.endOffsets.last < newText.size) {
      // Only perform this if it isn't part of a word.  A space is most reliable.
      if (newText(s.endOffsets.last) == ' ')
        newText = newText.updated(s.endOffsets.last, '\n')
    }
    newText
  }
}

object FilterByLength {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def apply(processor: Processor, cutoff: Int = 200): FilterByLength = new FilterByLength(processor, cutoff)
}

