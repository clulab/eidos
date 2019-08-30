package org.clulab.wm.eidos

import java.util.regex.Pattern

import org.antlr.v4.runtime.CommonTokenStream
import org.clulab.processors.Document
import org.clulab.processors.Processor
import org.clulab.processors.Sentence
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.clu.PortugueseCluProcessor
import org.clulab.processors.clu.SpanishCluProcessor
import org.clulab.processors.clu.tokenizer.EnglishSentenceSplitter
import org.clulab.processors.clu.tokenizer.OpenDomainEnglishLexer
import org.clulab.processors.clu.tokenizer.OpenDomainEnglishTokenizer
import org.clulab.processors.clu.tokenizer.OpenDomainPortugueseTokenizer
import org.clulab.processors.clu.tokenizer.OpenDomainPortugueseTokenizerLexer
import org.clulab.processors.clu.tokenizer.OpenDomainSpanishTokenizer
import org.clulab.processors.clu.tokenizer.OpenDomainSpanishTokenizerLexer
import org.clulab.processors.clu.tokenizer.PortugueseSentenceSplitter
import org.clulab.processors.clu.tokenizer.RawToken
import org.clulab.processors.clu.tokenizer.SentenceSplitter
import org.clulab.processors.clu.tokenizer.SpanishSentenceSplitter
import org.clulab.processors.clu.tokenizer.Tokenizer
import org.clulab.processors.clu.tokenizer.TokenizerLexer
import org.clulab.processors.clu.tokenizer.TokenizerStep
import org.clulab.processors.clu.tokenizer.TokenizerStepAccentedNormalization
import org.clulab.processors.clu.tokenizer.TokenizerStepContractions
import org.clulab.processors.clu.tokenizer.TokenizerStepNormalization
import org.clulab.processors.clu.tokenizer.TokenizerStepPortugueseContractions
import org.clulab.processors.clu.tokenizer.TokenizerStepSpanishContractions
import org.clulab.processors.corenlp.CoreNLPDocument
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

trait SentencesExtractor {
  def extractSentences(text: String): Array[Sentence]
}

class EidosTokenizerStep extends TokenizerStep {

  def process(inputs: Array[RawToken]): Array[RawToken] = {
    inputs
  }
}

class EidosProcessor(protected val processor: Processor, val tokenizer: Tokenizer, val language: String, cutoff: Int) extends Processor
    with SentencesExtractor {
  protected val regex: Regex = """(\.?)(\s*)\n(\s*)\n(\s*)""".r

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

    assert(text.length == newerText.length)
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

  protected def mkDocumentOriginal(text: String, keepText: Boolean): Document = processor.mkDocument(text, keepText)

  protected def mkDocumentFilter(text: String, keepText: Boolean): Document = {
    require(keepText)

    val filteredText = filterText(text)
    // Even if the text is being replaced right away and we'd like to use keepText = false,
    // some processors assert that keepText is true, so the shortcut is not taken.
    val document = processor.mkDocument(filteredText, keepText = true)
    val filteredDocument = filterSentences(document)

    filteredDocument.text = Some(text) // Use the original text here, even if it is empty.
    document
  }

  protected def mkDocumentParagraph(text: String, keepText: Boolean): Document = {
    require(keepText)

    // Each of the possible processor types ends up calling this in the end.
    val preDocument = CluProcessor.mkDocument(tokenizer, text, keepText)
    val document = if (processor.isInstanceOf[ShallowNLPProcessor])
      ShallowNLPProcessor.cluDocToCoreDoc(preDocument, keepText)
    else
      preDocument
    val filteredDocument = filterSentences(document)

    filteredDocument
  }

  override def mkDocument(text: String, keepText: Boolean): Document = {
//    mkDocumentOriginal(text, keepText)
//    mkDocumentFilter(text, keepText)
    mkDocumentParagraph(text, keepText)
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

class LanguagePack(val lexer: TokenizerLexer, val contractionStep: TokenizerStep,
    val normalizationStep: TokenizerStepNormalization, val sentenceSplitter: SentenceSplitter)

class EnglishLanguagePack extends LanguagePack(
  new OpenDomainEnglishLexer,
  new TokenizerStepContractions,
  new TokenizerStepNormalization,
  new EnglishSentenceSplitter
)

class PortugueseLanguagePack extends LanguagePack(
  new OpenDomainPortugueseTokenizerLexer,
  new TokenizerStepPortugueseContractions,
  new TokenizerStepAccentedNormalization,
  new PortugueseSentenceSplitter
)

class SpanishLanguagePack extends LanguagePack(
  new OpenDomainSpanishTokenizerLexer,
  new TokenizerStepSpanishContractions,
  new TokenizerStepAccentedNormalization,
  new SpanishSentenceSplitter
)

class EidosTokenizerLexer(val tokenizerLexer: TokenizerLexer) extends TokenizerLexer {

  override def mkLexer(text: String): CommonTokenStream = {
    val commonTokenStream = tokenizerLexer.mkLexer(text)
    // Now do something more with text and common token stream, like add periods appropriately for whitespace
    commonTokenStream
  }
}

class ParagraphSplitter {
  // The idea here is to make sure that a paragraph ends with a complete sentence.
  // A paragraph is demarcated by two linefeeds (eopPattern below) between two other tokens.
  // Neither of the other tokens should be end of sentence (eosPattern) characters themselves.
  // At this stage periods have not been combined with their abbreviations, so they are separate.
  def split(text: String, tokens: Array[RawToken]): Array[RawToken] = {
    val hasEoses = tokens.map { token => ParagraphSplitter.eosPattern.matcher(token.word).matches }
    val newTokens = new ArrayBuffer[RawToken]()

    tokens.indices.foreach { index =>
      val prevToken = tokens(index)
      val nextTokenOpt = if (index + 1 < tokens.length) Some(tokens(index + 1)) else None
      val hasEos = hasEoses(index) || (nextTokenOpt.isDefined && hasEoses(index + 1))

      newTokens += prevToken
      if (!hasEos) {
        val beginPosition = prevToken.endPosition
        val endPosition =
            if (nextTokenOpt.isDefined) nextTokenOpt.get.beginPosition
            else text.length
        val whitespace = text.slice(beginPosition, endPosition)
        val hasEop = ParagraphSplitter.eopPattern.matcher(whitespace).matches

        if (hasEop) {
          // The sentenceSplitter could refrain from using this period in an abbreviation
          // by noticing that the raw text does not make sense in that context.
          val newToken = new RawToken(whitespace, beginPosition, endPosition, ".")
          newTokens += newToken
        }
      }
    }
    newTokens.toArray
  }
}

object ParagraphSplitter {
  val eosPattern: Pattern = SentenceSplitter.EOS.pattern // End of sentence, that is.
  val eopPattern: Pattern = """^(\.?)(\s*)\n(\s*)\n(\s*)$""".r.pattern // End of paragraph
}

class EidosTokenizer(languagePack: LanguagePack) extends Tokenizer(
  new EidosTokenizerLexer(languagePack.lexer),
  Seq(languagePack.contractionStep, languagePack.normalizationStep),
  languagePack.sentenceSplitter
) {
  protected val lexer: EidosTokenizerLexer = new EidosTokenizerLexer(languagePack.lexer) // Now we have two of them
  protected val steps: Seq[TokenizerStep] = Seq(languagePack.contractionStep, languagePack.normalizationStep)
  protected val sentenceSplitter: SentenceSplitter = languagePack.sentenceSplitter
  protected val paragraphSplitter: ParagraphSplitter = new ParagraphSplitter

  /** Tokenization and sentence splitting */
  override def tokenize(text:String, sentenceSplit:Boolean = true):Array[Sentence] = {
    val tokens: CommonTokenStream = lexer.mkLexer(text)
    var done = false

    val rawTokens = new ArrayBuffer[RawToken]()

    //
    // raw tokenization, using the antlr grammar
    //
    while(! done) {
      val t = tokens.LT(1)
      if(t.getType == -1) {
        // EOF
        done = true
      } else {
        // info on the current token
        val word = t.getText
        val beginPosition = t.getStartIndex
        val endPosition = t.getStopIndex + 1 // antlr is inclusive on end position, we are exclusive

        // make sure character positions are legit
        assert(beginPosition + word.length == endPosition)

        // add to raw stream
        rawTokens += RawToken(word, beginPosition)

        // advance to next token in stream
        tokens.consume()
      }
    }

    //
    // now apply all the additional non-Antlr steps such as solving contractions, normalization, post-processing
    //
    var postProcessedTokens = rawTokens.toArray
    for(step <- steps) {
      postProcessedTokens = step.process(postProcessedTokens)
    }

    // The major change is here!
    postProcessedTokens = paragraphSplitter.split(text, postProcessedTokens)
    //
    // sentence splitting, including detection of abbreviations
    //
    sentenceSplitter.split(postProcessedTokens, sentenceSplit)
  }
}

object EidosProcessor {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def apply(language: String, cutoff: Int = 200): EidosProcessor = {
    val (processor, tokenizer) = language match {
      case "english" =>
        val processor = new FastNLPProcessor
        val standardTokenizer = processor.tokenizer
        assert(standardTokenizer.isInstanceOf[OpenDomainEnglishTokenizer])
        val customTokenizer = new EidosTokenizer(new EnglishLanguagePack)
        (processor, customTokenizer)
      case "spanish" =>
        val processor = new SpanishCluProcessor
        val standardTokenizer = processor.tokenizer
        assert(standardTokenizer.isInstanceOf[OpenDomainSpanishTokenizer])
        val customTokenizer = new EidosTokenizer(new SpanishLanguagePack)
        (processor, customTokenizer)
      case "portuguese" =>
        val processor = new PortugueseCluProcessor
        val standardTokenizer = processor.tokenizer
        assert(standardTokenizer.isInstanceOf[OpenDomainPortugueseTokenizer])
        val customTokenizer = new EidosTokenizer(new PortugueseLanguagePack)
        (processor, customTokenizer)
    }
    new EidosProcessor(processor, tokenizer, language, cutoff)
  }
}
