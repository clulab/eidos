package org.clulab.wm.eidos.context

import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.net.URL
import java.nio.file.{Files, Path, Paths}

import scala.collection.JavaConverters._
import ai.lum.common.ConfigUtils._
import com.google.common.io.ByteStreams
import com.typesafe.config.Config
import de.bwaldvogel.liblinear.{Feature, FeatureNode, Linear, Model, Parameter, Problem, SolverType}
import org.clulab.odin.{Mention, State, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.struct.Interval
import org.clulab.utils.Closer._
import org.clulab.wm.eidos.attachments.Location
import org.clulab.wm.eidos.document.EidosDocument
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.utils.FileUtils
import org.slf4j.LoggerFactory
import org.tensorflow.{Graph, Session, Tensor}

import scala.collection.mutable

object GeoNormFinder {

  private lazy val logger = LoggerFactory.getLogger(getClass)

  def fromConfig(config: Config): GeoNormFinder = {
    val geoNamesIndexPath = Paths.get(config[String]("geoNamesIndexPath"))
    val geoNamesIndexURL: URL = config[URL]("geoNamesIndexURL")

    // if the index is not yet local, download it
    if (!Files.exists(geoNamesIndexPath.resolve("segments_1"))) {
      logger.info(s"No GeoNames index at $geoNamesIndexPath.")

      // copy the zip file to the local machine
      logger.info(s"Downloading the GeoNames index from $geoNamesIndexURL.")
      val zipPath = geoNamesIndexPath.resolve("geonames-index.zip")
      Files.createDirectories(geoNamesIndexPath)
      Files.copy(geoNamesIndexURL.openStream, zipPath)

      // unzip the zip file
      logger.info(s"Extracting the GeoNames index to $geoNamesIndexPath.")
      FileUtils.unzip(zipPath, geoNamesIndexPath)

      // remove the downloaded zip file
      Files.delete(zipPath)
    }

    def getStream(path: String) = getClass.getResourceAsStream(config[String](path))

    new GeoNormFinder(
      new GeoLocationExtractor(
        GeoLocationExtractor.loadNetwork(getStream("taggerModelPath")),
        GeoLocationExtractor.loadVocabulary(getStream("taggerWordToIndexPath"))),
      new GeoLocationNormalizer(
        new GeoNamesSearcher(geoNamesIndexPath),
        Some(GeoLocationNormalizer.loadModel(getStream("normalizerModelPath")))))
  }
}

class GeoNormFinder(extractor: GeoLocationExtractor, normalizer: GeoLocationNormalizer) extends Finder {
  override def extract(doc: Document, initialState: State): Seq[Mention] = doc match {
    case eidosDoc: EidosDocument =>
      val sentenceBuffers = eidosDoc.sentences.map(_ => mutable.Buffer.empty[GeoPhraseID])
      eidosDoc.geolocs = Some(sentenceBuffers.map(_.toSeq))
      val sentenceLocations = extractor(eidosDoc.sentences.map(_.raw))
      val Some(text) = eidosDoc.text
      for {
        sentenceIndex <- eidosDoc.sentences.indices
        sentence = eidosDoc.sentences(sentenceIndex)
        locations = sentenceLocations(sentenceIndex)
        (wordStartIndex, wordEndIndex) <- locations
      } yield {
        val charStartIndex = sentence.startOffsets(wordStartIndex)
        val charEndIndex = sentence.endOffsets(wordEndIndex - 1)
        val locationPhrase = text.substring(charStartIndex, charEndIndex)
        val geoID = normalizer(text, (charStartIndex, charEndIndex)).headOption.map {
          case (entry, _) => entry.id.toInt
        }
        val geoPhraseID = GeoPhraseID(locationPhrase, geoID, charStartIndex, charEndIndex)
        sentenceBuffers(sentenceIndex) += geoPhraseID
        new TextBoundMention(
          Seq("Location"),
          Interval(wordStartIndex, wordEndIndex),
          sentenceIndex,
          eidosDoc,
          true,
          getClass.getSimpleName(),
          Set(Location(geoPhraseID))
        )
      }
  }
}


object GeoLocationExtractor {
  val I_LOC = 0
  val B_LOC = 1
  val O_LOC = 2
  val PADDING_TOKEN = "PADDING_TOKEN"
  val UNKNOWN_TOKEN = "UNKNOWN_TOKEN"
  val TF_INPUT = "words_input"
  val TF_OUTPUT = "time_distributed_1/Reshape_1"


  def loadNetwork(inputStream: InputStream): Session = {
    val graph = new Graph
    graph.importGraphDef(ByteStreams.toByteArray(inputStream))
    new Session(graph)
  }

  def loadVocabulary(inputStream: InputStream): Map[String, Int] = {
    val reader = new BufferedReader(new InputStreamReader(inputStream))
    val wordToIndex = reader.lines.iterator.asScala.map(_.split(' ')).map {
      case Array(word, n) => (word, n.toInt)
    }
    wordToIndex.toMap
  }
}

class GeoLocationExtractor(taggerModel: Session, wordToIndex: Map[String, Int]) {

  lazy private val unknownInt = wordToIndex(GeoLocationExtractor.UNKNOWN_TOKEN)
  lazy private val paddingInt = wordToIndex(GeoLocationExtractor.PADDING_TOKEN)

  /**
    * Finds locations in a document.
    *
    * @param sentenceWords A series of sentences, where each sentence is an array of word Strings.
    * @return For each input sentence, a list of locations, where each location is a begin word,
    *         and end word (exclusive).
    */
  def apply(sentenceWords: Array[Array[String]]): Array[Seq[(Int, Int)]] = if (sentenceWords.isEmpty) Array.empty else {
    // get the word-level features, and pad to the maximum sentence length
    val maxLength = sentenceWords.map(_.length).max
    val sentenceFeatures = for (words <- sentenceWords) yield {
      words.map(wordToIndex.getOrElse(_, this.unknownInt)).padTo(maxLength, this.paddingInt)
    }

    // feed the word features through the Tensorflow model
    import GeoLocationExtractor.{TF_INPUT, TF_OUTPUT}
    val input = Tensor.create(sentenceFeatures)
    val Array(output: Tensor[_]) = taggerModel.runner().feed(TF_INPUT, input).fetch(TF_OUTPUT).run().toArray

    // convert word-level probability distributions to word-level class predictions
    val Array(nSentences, nWords, nClasses) = output.shape
    val sentenceWordProbs = Array.ofDim[Float](nSentences.toInt, nWords.toInt, nClasses.toInt)
    output.copyTo(sentenceWordProbs)
    val sentenceWordPredictions: Array[Seq[Int]] = sentenceWordProbs.map(_.map{ wordProbs =>
      val max = wordProbs.max
      wordProbs.indexWhere(_ == max)
    }.toSeq)

    // convert word-level class predictions into span-level geoname predictions
    import GeoLocationExtractor.{B_LOC, I_LOC, O_LOC}
    for ((words, paddedWordPredictions) <- sentenceWords zip sentenceWordPredictions) yield {
      // trim off any predictions on padding words
      val wordPredictions = paddedWordPredictions.take(words.length)
      for {
        (wordPrediction, wordIndex) <- wordPredictions.zipWithIndex

        // a start is either a B, or an I that is following an O
        if wordPrediction == B_LOC || (wordPrediction == I_LOC &&
          // an I at the beginning of a sentence is not considered to be a start,
          // since as of Jun 2019, such tags seemed to be mostly errors (non-locations)
          wordIndex != 0 && wordPredictions(wordIndex - 1) == O_LOC)
      } yield {

        // the end index is the first B or O (i.e., non-I) following the start
        val end = wordPredictions.indices.indexWhere(wordPredictions(_) != I_LOC, wordIndex + 1)
        val endIndex = if (end == -1) words.length else end

        // yield the token span
        (wordIndex, endIndex)
      }
    }
  }

}


object GeoLocationNormalizer {

  def loadModel(inputStream: InputStream): Model = {
    new BufferedReader(new InputStreamReader(inputStream)).autoClose(Model.load)
  }

  def loadModel(path: Path): Model = {
    Files.newBufferedReader(path).autoClose(Model.load)
  }

  def train(searcher: GeoNamesSearcher,
            trainingData: Iterator[(String, Seq[(Int, Int)], Seq[String])]): GeoLocationNormalizer = {
    val reranker = new GeoLocationNormalizer(searcher)

    // convert training data into features and labels
    val featureLabels: Iterator[(Array[Feature], Double)] = for {
      (text, spans, geoIDs) <- trainingData
      (span, geoID) <- spans zip geoIDs
      scoredEntries = reranker.scoredEntries(text, span)

      // pair each correct entry with all incorrect ones
      correctIndices = scoredEntries.indices.filter(i => scoredEntries(i)._1.id == geoID).toSet
      correctIndex <- correctIndices
      incorrectIndex <- scoredEntries.indices
      if !correctIndices.contains(incorrectIndex)

      // include the pair in both orders (correct first, and correct second)
      labeledFeatures <- Seq(
        (reranker.pairFeatures(scoredEntries(correctIndex), scoredEntries(incorrectIndex)), 1.0),
        (reranker.pairFeatures(scoredEntries(incorrectIndex), scoredEntries(correctIndex)), 0.0)
      )
    } yield {
      labeledFeatures
    }

    // feed the features and labels into liblinear
    val (features, labels) = featureLabels.toArray.unzip
    val problem = new Problem
    problem.x = features
    problem.y = labels
    problem.l = labels.length
    problem.n = 2 + featureCodePairIndex.size + 1
    problem.bias = 1

    // use a logistic regression model since we need probabilities
    val param = new Parameter(SolverType.L1R_LR, 1.0, 0.01)
    val model = Linear.train(problem, param)

    // return the trained reranking model
    new GeoLocationNormalizer(searcher, Some(model))
  }

  // country, state, region, ... and city, village, ... from http://www.geonames.org/export/codes.html
  private val featureCodes = """
    ADM1 ADM1H ADM2 ADM2H ADM3 ADM3H ADM4 ADM4H ADM5 ADMD ADMDH LTER PCL PCLD PCLF PCLH PCLI PCLIX PCLS PRSH TERR ZN ZNB
    PPL PPLA PPLA2 PPLA3 PPLA4 PPLC PPLCH PPLF PPLG PPLH PPLL PPLQ PPLR PPLS PPLW PPLX STLMT
    """.split("""\s+""")

  private val featureCodeSet = featureCodes.toSet

  private def featureCode(entry: GeoNamesEntry): String = {
    if (featureCodeSet.contains(entry.featureCode)) entry.featureCode else "UNK"
  }

  private val featureCodePairIndex: Map[(String, String), Int] = {
    val pairs = for (x <- featureCodes ++ Array("UNK"); y <- featureCodes ++ Array("UNK")) yield (x, y)
    pairs.zipWithIndex.toMap
  }
}


class GeoLocationNormalizer(searcher: GeoNamesSearcher, linearModel: Option[Model] = None) {

  def scoredEntries(text: String, span: (Int, Int)): Seq[(GeoNamesEntry, Float)] = span match {
    case (start, end) => searcher(text.substring(start, end), 5)
  }

  private def pairFeatures(entryScore1: (GeoNamesEntry, Float), entryScore2: (GeoNamesEntry, Float)): Array[Feature] = {
    val (entry1, score1) = entryScore1
    val (entry2, score2) = entryScore2
    val featureCodePair = (GeoLocationNormalizer.featureCode(entry1), GeoLocationNormalizer.featureCode(entry2))
    Array[Feature](
      // difference in retrieval scores
      new FeatureNode(1, score1 - score2),
      // difference in log-populations
      new FeatureNode(2, math.log10(entry1.population + 1) - math.log10(entry2.population + 1)),
      // the pair of feature types, e.g., (ADM1, ADM2)
      new FeatureNode(3 + GeoLocationNormalizer.featureCodePairIndex(featureCodePair), 1))
  }

  def apply(text: String, span: (Int, Int)): Seq[(GeoNamesEntry, Float)] = {
    linearModel match {
      // if there is no model, rely on the searcher's order
      case None => this.scoredEntries(text, span)
      // if there is a model, rerank the search results
      case Some(model) =>
        // determine which index liblinear is using for the positive class
        val index1 = model.getLabels.indexOf(1)
        val scoredEntries = this.scoredEntries(text, span)

        // count the number of times an entry "wins" according to the pair-wise classifier
        val wins = Array.fill(scoredEntries.length)(0f)
        for (i <- scoredEntries.indices; j <- scoredEntries.indices; if i != j) {
          val probabilities = Array(0.0, 0.0)
          Linear.predictProbability(model, pairFeatures(scoredEntries(i), scoredEntries(j)), probabilities)

          // only count a "win" if the model is confident (threshold set manually using train/dev set)
          if (probabilities(index1) > 0.8)
            wins(i) += 1
        }

        // sort entries by the number of wins
        scoredEntries.zipWithIndex.map{
          case ((entry, _), i) => (entry, wins(i))
        }.sortBy(-_._2)
    }
  }

  def save(modelPath: Path): Unit = {
    for (model <- linearModel) {
      val writer = Files.newBufferedWriter(modelPath)
      model.save(writer)
      writer.close()
    }
  }
}
