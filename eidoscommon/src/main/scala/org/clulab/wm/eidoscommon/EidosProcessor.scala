package org.clulab.wm.eidoscommon

import java.text.Normalizer
import java.util.regex.Pattern

import org.clulab.dynet.Utils.initializeDyNet
import org.clulab.processors.Document
import org.clulab.processors.Processor
import org.clulab.processors.Sentence
import org.clulab.processors.clu.PortugueseCluProcessor
import org.clulab.processors.clu.SpanishCluProcessor
import org.clulab.processors.clu.tokenizer.RawToken
import org.clulab.processors.clu.tokenizer.SentenceSplitter
import org.clulab.processors.clu.tokenizer.Tokenizer
import org.clulab.processors.fastnlp.FastNLPProcessorWithSemanticRoles
import org.clulab.utils.ScienceUtils
import org.clulab.wm.eidoscommon.utils.Logging

import scala.collection.mutable.ArrayBuffer

trait EidosTokenizing {
  val eidosTokenizer: EidosTokenizer
}

trait EidosProcessor extends Processor with SentencesExtractor with LanguageSpecific with Tokenizing with EidosTokenizing

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

  def getTagSet: TagSet
}

class EidosEnglishProcessor(val language: String, cutoff: Int) extends FastNLPProcessorWithSemanticRoles
    with EidosProcessor {

  initializeDyNet()
  lazy val eidosTokenizer: EidosTokenizer = new EidosTokenizer(localTokenizer, cutoff)
  override lazy val tokenizer: Tokenizer = eidosTokenizer
  val tagSet = new EnglishTagSet()

  def getTokenizer: EidosTokenizer = eidosTokenizer

  // TODO: This should be checked with each update of processors.
  def extractDocument(text: String): Document = {
    // This mkDocument will now be subject to all of the EidosProcessor changes.
    val document = mkDocument(text, keepText = false)

    if (document.sentences.nonEmpty) {
      tagPartsOfSpeech(document)
      lemmatize(document)
      recognizeNamedEntities(document)
      srl(document)
    }
    document
  }

  def getTagSet: TagSet = tagSet
}

class EidosSpanishProcessor(val language: String, cutoff: Int) extends SpanishCluProcessor
    with EidosProcessor {
  lazy val eidosTokenizer: EidosTokenizer = new EidosTokenizer(localTokenizer, cutoff)
  override lazy val tokenizer: Tokenizer = eidosTokenizer
  val tagSet = new SpanishTagSet()

  def getTokenizer: EidosTokenizer = eidosTokenizer

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

  def getTagSet: TagSet = tagSet
}

class EidosPortugueseProcessor(val language: String, cutoff: Int) extends PortugueseCluProcessor
    with EidosProcessor {
  lazy val eidosTokenizer: EidosTokenizer = new EidosTokenizer(localTokenizer, cutoff)
  override lazy val tokenizer: Tokenizer = eidosTokenizer
  val tagSet = new PortugueseTagSet()

  def getTokenizer: EidosTokenizer = eidosTokenizer

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

  def getTagSet: TagSet = tagSet
}

class EidosCluProcessor(val language: String, cutoff: Int) extends FastNLPProcessorWithSemanticRoles
  with EidosProcessor {
  lazy val eidosTokenizer: EidosTokenizer = new EidosTokenizer(localTokenizer, cutoff)
  override lazy val tokenizer: Tokenizer = eidosTokenizer
  val tagSet = new EnglishTagSet()

  def getTokenizer: EidosTokenizer = eidosTokenizer

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

  def getTagSet: TagSet = tagSet
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

  def copyWithNewTokenizer(tokenizer: Tokenizer): EidosTokenizer = new EidosTokenizer(tokenizer, cutoff)

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
          Seq((' ', (start, end))) // This will change word boundaries!
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
            // The paragraph splitter may have added tokens with positions beyond the string
            // boundaries and therefore beyond the boundaries of the sanitized ranges.
            val newBeginPosition =
                if (oldBeginPosition < sanitizedRanges.length)
                  sanitizedRanges(oldBeginPosition)._1
                else
                  text.length
            val newEndPosition =
                if (oldEndPosition < sanitizedRanges.length)
                  // This might more typically be (oldEndPosition)._1, and usually that would give
                  // the same answer, but if characters have been deleted it might not be.
                  sanitizedRanges(oldEndPosition - 1)._2
                else
                  text.length
            val newRaw = text.slice(newBeginPosition, newEndPosition)

            RawToken(newRaw, newBeginPosition, newEndPosition, word)
          }

    paragraphTokens
  }

  def ensentence(tokens: Array[RawToken], sentenceSplit: Boolean): Array[Sentence] = {
    // split() looks only at the word, not the token, so it is safe to have
    // rewritten the tokens at this point.
    val sentences = sentenceSplitter.split(tokens, sentenceSplit)
    val reasonableSentences = sentences
      // The second change is to filter by sentence length.
      .filter { sentence => sentence.words.length < cutoff }
      // This is to filter out tables/graphs/etc mis-parsed as text.
      .filterNot { sentence => isFubar(sentence) }
    val skipLength = sentences.length - reasonableSentences.length

    if (skipLength > 0)
      EidosProcessor.logger.info(s"skipping $skipLength sentences")
    reasonableSentences
  }

  // This is a bit misnamed, but we are overriding a processors method here.
  override def tokenize(text: String, sentenceSplit: Boolean = true): Array[Sentence] = {
    val tokens = entoken(text)
    val sentences = ensentence(tokens, sentenceSplit)

    sentences
  }

  // Determines if the sentence is likely a misparsed table or graph etc., either because it has too many
  // numbers in it or if there are several single character words.
  private def isFubar(sentence: Sentence): Boolean = {
    def hasAlpha(s: String): Boolean = s.exists(_.isLetter)
    // This lazy is an experiment related to the short-circuit ||.  Why calculate if it won't make a difference?
    lazy val numNonAlpha = sentence.words.count(!hasAlpha(_))
    lazy val numSingleAlpha = sentence.words.count { word => word.length == 1 && word.head.isLetter }
    val fubar = false ||
        numNonAlpha >= EidosProcessor.MAX_NON_ALPHA ||
        numSingleAlpha > EidosProcessor.MAX_SINGLE_ALPHA

    fubar
  }
}

object EidosTokenizer {
  val (unicodes: Map[Char, String], accents: Set[Char]) = {
    val scienceUtils = new ScienceUtils()

    (scienceUtils.unicodes, scienceUtils.accents)
  }
}

trait Tokenizing {
  def getTokenizer: EidosTokenizer
}

object EidosProcessor extends Logging {
  val DEFAULT_CUTOFF = 200
  val MAX_NON_ALPHA = 10
  val MAX_SINGLE_ALPHA = 5

  def apply(language: String, cutoff: Int = DEFAULT_CUTOFF): EidosProcessor = language match {
    case Language.ENGLISH =>
      new EidosEnglishProcessor(language, cutoff)
    case Language.SPANISH => new EidosSpanishProcessor(language, cutoff)
    case Language.PORTUGUESE => new EidosPortugueseProcessor(language, cutoff)
    case Language.CLU => new EidosCluProcessor(language, cutoff)
  }

  // Turn off warnings from this class.
  edu.stanford.nlp.ie.NumberNormalizer.setVerbose(false)
}
