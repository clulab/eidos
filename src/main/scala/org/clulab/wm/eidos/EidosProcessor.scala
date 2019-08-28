package org.clulab.wm.eidos

import org.clulab.processors.Document
import org.clulab.processors.Processor
import org.clulab.processors.Sentence
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.clu.PortugueseCluProcessor
import org.clulab.processors.clu.SpanishCluProcessor
import org.clulab.processors.corenlp.CoreNLPDocument
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.util.matching.Regex

trait SentencesExtractor {
  def extractSentences(text: String): Array[Sentence]
}

class EidosProcessor(protected val processor: Processor, val language: String, cutoff: Int) extends Processor with SentencesExtractor {
  protected val regex: Regex = """(\.?)(\s)*\n(\s)*\n(\s)*""".r

  // This code originated in TreeDomainOntology which needs to partially process sentences through the NER stage.
  // It is slightly convoluted so that the type of processor need only be checked once and not on every call.
  // It is assumed that the incoming String does not need to be filtered.
  protected val getSentences: String => Array[Sentence] = processor match {
    // Earlier, a complete annotation was performed.
    // val sentences = proc.annotate(text).sentences
    // Now we just go through the POS tagging stage, but the procedure is
    // different for different kinds of processors.
    // CluProcessor is the superclass of both SpanishCluProcessor and PortugueseCluProcessor.
    case processor: CluProcessor => text => {
      val document = processor.mkDocument(text)

      // This is the key difference.  Lemmatization must happen first.
      processor.lemmatize(document)
      processor.tagPartsOfSpeech(document)
      processor.recognizeNamedEntities(document)
      document.sentences
    }
    case processor: ShallowNLPProcessor => text => {
      val document = processor.mkDocument(text)

      if (document.sentences.nonEmpty) {
        processor.tagPartsOfSpeech(document)
        processor.lemmatize(document)
        processor.recognizeNamedEntities(document)
      }
      document.sentences
    }
  }

  def extractSentences(text: String): Array[Sentence] = getSentences(text)

  def filterText(text: String): String = {
    val newText = regex.replaceAllIn(text, { regexMatch: Regex.Match =>
      def groupLength(index: Int): Int = Option(regexMatch.group(index)).map(_.length).getOrElse(0)
      val hasPeriod = regexMatch.group(1).nonEmpty
      val spaceCount = groupLength(2) + groupLength(3) + groupLength(4)

      if (hasPeriod) {
        // Just arrange the newlines before the spaces, if only to keep things neat.
        // Would it be better to preserve the original order?
        ".\n\n" + (" " * spaceCount)
      }
      else if (spaceCount >= 2) {
        // Insert a period, but after a protective space, then reused the newlines and finish with two fewer spaces.
        " .\n\n" + (" " * (spaceCount - 2))
      }
      else if (spaceCount == 1) {
        // Move the space ahead of the period and insert a period in place of one newline.
        " .\n"
      }
      else {
        // Insert a period, but before a protective space, and that's all there is room for.
        // This space may not be on the best side, but other components are looking out for
        // abbreviations and may be confused by a period in front of the next word.
        ". "
      }
    })
    // This is originally from DocumentFilter, but it is now being applied before tokenization.
    val newerText = newText
        .replace('\n', ' ')
        .replace('\f', ' ')

    if (text.length != newerText.length)
      println("What happened?")
//    assert(text.length == newerText.length)
    newerText
  }

  def filterSentences(document: Document): Document = {
    // This happens so rarely that it's not worth filtering when not necessary.
    if (document.sentences.indexWhere(_.words.length >= cutoff) >= 0) {
      val keptSentences = document.sentences.filter(sentence => !(sentence.words.length >= cutoff))
      val skipLength = document.sentences.length - keptSentences.length
      val newDocument = Document(document.id, keptSentences, document.coreferenceChains, document.discourseTree, document.text)
      val newerDocument = // This is a hack for lack of copy constructor for CoreNLPDocument
        if (document.isInstanceOf[CoreNLPDocument])
          ShallowNLPProcessor.cluDocToCoreDoc(newDocument, keepText = document.text.isDefined)
        else
          newDocument

      EidosProcessor.logger.info(s"skipping $skipLength sentences")
      newerDocument
    }
    else
      document
  }

  override def mkDocument(text: String, keepText: Boolean): Document = {
    require(keepText)

    val filteredText = filterText(text)
    val document = processor.mkDocument(filteredText, keepText = false)
    val filteredDocument = filterSentences(document)

    filteredDocument.text = Some(text) // Use the original text here.
    document
  }

  override def mkDocumentFromSentences(sentences: Iterable[String], keepText: Boolean, charactersBetweenSentences: Int): Document =
      processor.mkDocumentFromSentences(sentences, keepText)

  override def mkDocumentFromTokens(sentences: Iterable[Iterable[String]], keepText: Boolean, charactersBetweenSentences: Int, charactersBetweenTokens: Int): Document =
      processor.mkDocumentFromTokens(sentences, keepText, charactersBetweenSentences, charactersBetweenTokens)

  override def tagPartsOfSpeech(doc: Document): Unit =
      processor.tagPartsOfSpeech(doc)

  override def lemmatize(doc: Document): Unit =
      processor.lemmatize(doc)

  override def recognizeNamedEntities(doc: Document): Unit =
      processor.recognizeNamedEntities(doc)

  override def parse(doc: Document): Unit =
      processor.parse(doc)

  override def chunking(doc: Document): Unit =
      processor.chunking(doc)

  override def resolveCoreference(doc: Document): Unit =
      processor.resolveCoreference(doc)

  override def discourse(doc: Document): Unit =
      processor.discourse(doc)

  override def relationExtraction(doc: Document): Unit =
      processor.relationExtraction(doc)
}

object EidosProcessor {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def apply(language: String, cutoff: Int = 200): EidosProcessor = {
    val processor = language match {
      case "english" => new FastNLPProcessor
      case "spanish" => new SpanishCluProcessor
      case "portuguese" => new PortugueseCluProcessor
    }
    new EidosProcessor(processor, language, cutoff)
  }
}
