package org.clulab.wm.eidos

import java.util.regex.Pattern

import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.Token
import org.clulab.processors.Processor
import org.clulab.processors.Sentence
import org.clulab.processors.clu.PortugueseCluProcessor
import org.clulab.processors.clu.SpanishCluProcessor
import org.clulab.processors.clu.tokenizer.RawToken
import org.clulab.processors.clu.tokenizer.SentenceSplitter
import org.clulab.processors.clu.tokenizer.Tokenizer
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer

// This interface is needed by the TreeDomainOntologyBuilder that wants sentences
// that are not quite as complete as the processors normally provide.
trait SentencesExtractor {
  def extractSentences(text: String): Array[Sentence]
}

// Allow the processors below to answer which language they are supporting.
trait LanguageSpecific {
  val language: String
}

class EidosEnglishProcessor(val language: String, cutoff: Int) extends FastNLPProcessor
    with SentencesExtractor with LanguageSpecific {
  override lazy val tokenizer = new EidosTokenizer(localTokenizer, cutoff)

  // TODO: Move this to processors.
  def extractSentences(text: String): Array[Sentence] = {
    // This mkDocument will now be subject to all of the EidosProcessor changes.
    val document = mkDocument(text, keepText = false)

    if (document.sentences.nonEmpty) {
      tagPartsOfSpeech(document)
      lemmatize(document)
      recognizeNamedEntities(document)
    }
    document.sentences
  }
}

class EidosSpanishProcessor(val language: String, cutoff: Int) extends SpanishCluProcessor
    with SentencesExtractor with LanguageSpecific {
  override lazy val tokenizer = new EidosTokenizer(localTokenizer, cutoff)

  // TODO: Move this to processors.
  def extractSentences(text: String): Array[Sentence] = {
    // This mkDocument will now be subject to all of the EidosProcessor changes.
    val document = mkDocument(text, keepText = false)

    if (document.sentences.nonEmpty) {
      lemmatize(document)
      tagPartsOfSpeech(document)
      recognizeNamedEntities(document)
    }
    document.sentences
  }
}

class EidosPortugueseProcessor(val language: String, cutoff: Int) extends PortugueseCluProcessor
    with SentencesExtractor with LanguageSpecific {
  override lazy val tokenizer = new EidosTokenizer(localTokenizer, cutoff)

  // TODO: Move this to processors.
  def extractSentences(text: String): Array[Sentence] = {
    // This mkDocument will now be subject to all of the EidosProcessor changes.
    val document = mkDocument(text, keepText = false)

    if (document.sentences.nonEmpty) {
      cheapLemmatize(document)
      tagPartsOfSpeech(document)
      recognizeNamedEntities(document)
    }
    document.sentences
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

class EidosTokenizer(tokenizer: Tokenizer, cutoff: Int) extends Tokenizer(
  tokenizer.lexer, tokenizer.steps, tokenizer.sentenceSplitter
) {
  val paragraphSplitter = new ParagraphSplitter()

  // TODO This should be inherited from a method in Tokenizer
  protected def readTokens(text: String): Array[RawToken] = {

    def newRawToken(token: Token): RawToken = {
      val word = token.getText
      val beginPosition = token.getStartIndex
      val endPosition = token.getStopIndex + 1 // antlr is inclusive on end position, we are exclusive

      assert(beginPosition + word.length == endPosition)
      RawToken(word, beginPosition)
    }

    val tokens: CommonTokenStream = lexer.mkLexer(text)
    val rawTokenBuffer = new ArrayBuffer[RawToken]()

    def processToken(token: Token): Boolean = {
      if (token.getType == -1)
        false
      else {
        rawTokenBuffer += newRawToken(token)
        true
      }
    }

    while (processToken(tokens.LT(1)))
      tokens.consume()
    rawTokenBuffer.toArray
  }

  // See Tokenizer for this method's template.
  override def tokenize(text: String, sentenceSplit: Boolean = true): Array[Sentence] = {
    val tokens = readTokens(text)
    val rawTokens = steps.foldLeft(tokens) { (tokens, step) =>
      step.process(tokens)
    }
    // The first major change is with the added paragraphSplitter.
    val paragraphTokens = paragraphSplitter.split(text, rawTokens)
    val sentences = sentenceSplitter.split(paragraphTokens, sentenceSplit)
    // The second change is to filter by sentence length.
    val shortSentences = sentences.filter { sentence => sentence.words.length < cutoff }
    val skipLength = sentences.length - shortSentences.length

    if (skipLength > 0)
      EidosProcessor.logger.info(s"skipping $skipLength sentences")
    shortSentences
  }
}

object EidosProcessor {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  type EidosProcessor = Processor with SentencesExtractor with LanguageSpecific

  def apply(language: String, cutoff: Int = 200): EidosProcessor = language match {
    case "english" => new EidosEnglishProcessor(language, cutoff)
    case "spanish" => new EidosSpanishProcessor(language, cutoff)
    case "portuguese" => new EidosPortugueseProcessor(language, cutoff)
  }
}
