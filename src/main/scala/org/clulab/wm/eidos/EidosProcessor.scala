package org.clulab.wm.eidos

import java.text.Normalizer
import java.util.regex.Pattern

import org.clulab.processors.Document
import org.clulab.processors.Processor
import org.clulab.processors.Sentence
import org.clulab.processors.clu.PortugueseCluProcessor
import org.clulab.processors.clu.SpanishCluProcessor
import org.clulab.processors.clu.tokenizer.RawToken
import org.clulab.processors.clu.tokenizer.SentenceSplitter
import org.clulab.processors.clu.tokenizer.Tokenizer
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.utils.ScienceUtils
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer

// This interface is needed by the TreeDomainOntologyBuilder that wants sentences
// that are not quite as complete as the processors normally provide.
trait SentencesExtractor {
  def extractDocument(text: String): Document

  def extractSentences(text: String): Array[Sentence] = {
    val document = extractDocument(text)

    document.sentences
  }
}

// Allow the processors below to answer which language they are supporting.
trait LanguageSpecific {
  val language: String
}

class EidosEnglishProcessor(val language: String, cutoff: Int) extends FastNLPProcessor
    with SentencesExtractor with LanguageSpecific {
  override lazy val tokenizer = new EidosTokenizer(localTokenizer, cutoff)

  // TODO: This should be checked with each update of processors.
  def extractDocument(text: String): Document = {
    // This mkDocument will now be subject to all of the EidosProcessor changes.
    val document = mkDocument(text, keepText = false)

    if (document.sentences.nonEmpty) {
      tagPartsOfSpeech(document)
      lemmatize(document)
      recognizeNamedEntities(document)
    }
    document
  }
}

class EidosSpanishProcessor(val language: String, cutoff: Int) extends SpanishCluProcessor
    with SentencesExtractor with LanguageSpecific {
  override lazy val tokenizer = new EidosTokenizer(localTokenizer, cutoff)

  // TODO: This should be checked with each update of processors.
  def extractDocument(text: String): Document = {
    // This mkDocument will now be subject to all of the EidosProcessor changes.
    val document = mkDocument(text, keepText = false)

    if (document.sentences.nonEmpty) {
      lemmatize(document)
      tagPartsOfSpeech(document)
      recognizeNamedEntities(document)
    }
    document
  }
}

class EidosPortugueseProcessor(val language: String, cutoff: Int) extends PortugueseCluProcessor
    with SentencesExtractor with LanguageSpecific {
  override lazy val tokenizer = new EidosTokenizer(localTokenizer, cutoff)

  // TODO: This should be checked with each update of processors.
  def extractDocument(text: String): Document = {
    // This mkDocument will now be subject to all of the EidosProcessor changes.
    val document = mkDocument(text, keepText = false)

    if (document.sentences.nonEmpty) {
      cheapLemmatize(document)
      tagPartsOfSpeech(document)
      recognizeNamedEntities(document)
    }
    document
  }
}

class ParagraphSplitter {
  // The idea here is to make sure that a paragraph ends with a complete sentence.
  // A paragraph is demarcated by two linefeeds (eopPattern below) between two other tokens.
  // Neither of the other tokens should be end of sentence (eosPattern) characters themselves.
  // At this stage periods have not been combined with their abbreviations, so they are separate.
  def split(text: String, tokens: Array[RawToken]): Array[RawToken] = {
    // See https://stackoverflow.com/questions/11391337/java-pattern-matcher-create-new-or-reset
    val eosMatcher = ParagraphSplitter.eosPattern.matcher("")
    val hasEoses = tokens.map { token => eosMatcher.reset(token.word).matches }
    val newTokens = new ArrayBuffer[RawToken]()
    val eopMatcher = ParagraphSplitter.eopPattern.matcher("")

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
        // Always end the document with EOP, especially since sentenceSplitter does add a period.
        val hasEop = eopMatcher.reset(whitespace).matches || nextTokenOpt.isEmpty

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
  // Since \n is a subset of \s, the greediness can lead to backtracking.
  // It will be more efficient to use (\s - \n)\n.
  val eopPattern: Pattern = """^(\.?)(\s*)\n(\s*)\n(\s*)$""".r.pattern // End of paragraph
  // However, timing tests showed that the efficiency claim above is untrue or at least not true enough.
  //val eopPattern: Pattern = """^(\.?)([ \t\x0B\f\r]*)\n([ \t\x0B\f\r]*)\n(\s*)$""".r.pattern // End of paragraph
}

class EidosTokenizer(tokenizer: Tokenizer, cutoff: Int) extends Tokenizer(
  tokenizer.lexer, tokenizer.steps, tokenizer.sentenceSplitter
) {
  val paragraphSplitter = new ParagraphSplitter()
  val form = Normalizer.Form.NFKC

  def normalize(oldText: String, oldRanges: Seq[(Int, Int)]): (String, Seq[(Int, Int)]) = {
    if (Normalizer.isNormalized(oldText, form))
      (oldText, oldRanges) // This should overshelmingly often be the case.
    else {
      val newTextAndRanges = oldText.zip(oldRanges).flatMap { case (char, (start, end)) =>
        val string = char.toString

        if (Normalizer.isNormalized(string, form))
          Seq((char, (start, end)))
        else {
          val text = Normalizer.normalize(string, form)

          assert(text.nonEmpty)
          text.map { char => (char, (start, end)) }
        }
      }

      val newText = newTextAndRanges.map(_._1).mkString
      val newRanges = newTextAndRanges.map(_._2)

      (newText, newRanges)
    }
  }

  def normalize(text: String): (String, Seq[(Int, Int)]) = {
    val ranges = text.indices.map { index => (index, index + 1) }

    normalize(text, ranges)
  }

  def isSanitized(text: String): Boolean =
      !text.exists { char => EidosTokenizer.unicodes.contains(char) || 0x80 <= char }

  def sanitize(oldText: String, oldRanges: Seq[(Int, Int)], keepAccents: Boolean = false): (String, Seq[(Int, Int)]) = {
    if (isSanitized(oldText))
      (oldText, oldRanges)
    else {
      val newTextAndRanges = oldText.zip(oldRanges).flatMap { case (char, (start, end)) =>
        val unicodeOpt = EidosTokenizer.unicodes.get(char)

        if (unicodeOpt.isDefined)
          if (keepAccents && EidosTokenizer.accents.contains(char))
            Seq((char, (start, end)))
          else
            unicodeOpt.get.map { char => (char, (start, end)) }
        else if (char < 0x80)
          Seq((char, (start, end)))
        else
          Seq((' ', (start, end))) // This will change work boundaries!
      }

      val newText = newTextAndRanges.map(_._1).mkString
      val newRanges = newTextAndRanges.map(_._2)

      (newText, newRanges)
    }
  }

  def sanitize(text: String, keepAccents: Boolean): (String, Seq[(Int, Int)]) = {
    val ranges = text.indices.map { index => (index, index + 1) }

    sanitize(text, ranges, keepAccents)
  }

  override protected def readTokens(text: String): Array[RawToken] = {
    val (normalizedText, normalizedRanges) = normalize(text)
    val (sanitizedText, sanitizedRanges) = sanitize(normalizedText, normalizedRanges, keepAccents = true)
    val redTokens = super.readTokens(sanitizedText)
    val rawTokens =
        if (text.eq(sanitizedText)) // If it is literally the same object...
          redTokens
        else
          redTokens.map { case RawToken(_, oldBeginPosition, oldEndPosition, word) =>
            val newBeginPosition = sanitizedRanges(oldBeginPosition)._1
            val newEndPosition = sanitizedRanges(oldEndPosition - 1)._2
            val newRaw = text.slice(newBeginPosition, newEndPosition)

            RawToken(newRaw, newBeginPosition, newEndPosition, word)
          }
    rawTokens
  }

  // Insentivize, break up into sentences
  // Entoken
  // Ensentence

  def entoken(text: String): Array[RawToken] = {
    val (normalizedText, normalizedRanges) = normalize(text)
    val (sanitizedText, sanitizedRanges) = sanitize(normalizedText, normalizedRanges, keepAccents = true)
    val rawTokens = readTokens(sanitizedText)
    val stepTokens = steps.foldLeft(rawTokens) { (rawTokens, step) =>
      step.process(rawTokens)
    }
    // This split() should be working on sanitizedText with any extra spaces in it
    // because it access what it thinks is the raw text in order to check for the spaces.
    // The first major change is with the added paragraphSplitter.
    val splitTokens = paragraphSplitter.split(sanitizedText, stepTokens)
    val paragraphTokens =
        if (text.eq(sanitizedText)) // If it is literally the same object...
          splitTokens
        else
          splitTokens.map { case RawToken(_, oldBeginPosition, oldEndPosition, word) =>
            val newBeginPosition = sanitizedRanges(oldBeginPosition)._1
            val newEndPosition = sanitizedRanges(oldEndPosition - 1)._2
            val newRaw = text.slice(newBeginPosition, newEndPosition)

            RawToken(newRaw, newBeginPosition, newEndPosition, word)
          }

    paragraphTokens
  }

  def ensentence(tokens: Array[RawToken], sentenceSplit: Boolean): Array[Sentence] = {
    // split() looks only at the word, not the token, so it is safe to have
    // rewritten the tokens at this point.
    val sentences = sentenceSplitter.split(tokens, sentenceSplit)
    // The second change is to filter by sentence length.
    val shortSentences = sentences.filter { sentence => sentence.words.length < cutoff }
    val skipLength = sentences.length - shortSentences.length

    if (skipLength > 0)
      EidosProcessor.logger.info(s"skipping $skipLength sentences")
    shortSentences
  }

  override def tokenize(text: String, sentenceSplit: Boolean = true): Array[Sentence] = {
    val tokens = entoken(text)
    val sentences = ensentence(tokens, sentenceSplit)

    sentences
  }
}

object EidosTokenizer {
  val (unicodes: Map[Char, String], accents: Set[Char]) = {
    val scienceUtils = new ScienceUtils()

    (scienceUtils.unicodes, scienceUtils.accents)
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
