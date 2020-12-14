package org.clulab.wm.eidos.context

import java.nio.file.{Files, Path, Paths}
import java.util.IdentityHashMap
import org.clulab.dynet.Metal
import org.clulab.dynet.Utils
import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.dynet.AnnotatedSentence
import org.clulab.geonorm.{GeoLocationExtractor, GeoLocationNormalizer, GeoNamesIndex}
import org.clulab.geonorm.{GeoLocationNormalizer, GeoNamesIndex}
import org.clulab.odin.{Mention, State, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.Location
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.mentions.OdinMention

import scala.collection.JavaConverters._

trait GeoExtractor {
  def extract(sentenceWords: Array[Array[String]]): Array[Array[(Int, Int)]]
}

class GeoNormGeoExtractor(geoLocationExtractor: GeoLocationExtractor) extends GeoExtractor {

  def extract(sentenceWords: Array[Array[String]]): Array[Array[(Int, Int)]] = geoLocationExtractor(sentenceWords)
}

class MetalGeoExtractor(metal: Metal) extends GeoExtractor {

  def extract(sentenceWords: Array[Array[String]]): Array[Array[(Int, Int)]] = {
    sentenceWords.map { words =>
      val annotatedSentence = new AnnotatedSentence(words)
      val predictions = metal.predict(0, annotatedSentence).toArray
      // convert word-level class predictions into span-level geoname predictions
      // trim off any predictions on padding words
      // val wordPredictions = paddedWordPredictions.take(words.length)
      for {
        (prediction, index) <- predictions.zipWithIndex

        // a start is either a B, or an I that is following an O
        if prediction == "B-LOC" || (prediction == "I-LOC" &&
            // an I at the beginning of a sentence is not considered to be a start,
            // since as of Jun 2019, such tags seemed to be mostly errors (non-locations)
            index != 0 && predictions(index - 1) == "O-LOC")
      } yield {
        // the end index is the first B or O (i.e., non-I) following the start
        val end = predictions.indices.indexWhere(predictions(_) != "I-LOC", index + 1)
        val endIndex = if (end == -1) words.length else end

        // yield the token span
        (index, endIndex)
      }
    }
  }
}

@SerialVersionUID(1L)
case class GeoPhraseID(text: String, geonameID: Option[String], startOffset: Int, endOffset: Int)

object GeoNormFinder {

  def fromConfig(config: Config): GeoNormFinder = {
    val geoExtractor = {
      val useMetal = config[Boolean]("useMetal")

      if (useMetal) {
        val modelFilenamePrefix = config[String]("modelFilenamePrefix")
        val metal = {
          Utils.initializeDyNet()
          Metal(modelFilenamePrefix)
        }

        new MetalGeoExtractor(metal)
      }
      else
        new GeoNormGeoExtractor(new GeoLocationExtractor())
      }
    val geoLocationNormalizer = {
      val geoNamesDir: Path = Paths.get(config[String]("geoNamesDir")).toAbsolutePath.normalize
      val geoNamesIndex =
        if (Files.exists(geoNamesDir) && Files.list(geoNamesDir).count() > 0)
          new GeoNamesIndex(geoNamesDir)
        else
          GeoNamesIndex.fromClasspathJar(geoNamesDir)

      new GeoLocationNormalizer(geoNamesIndex)
    }

    new GeoNormFinder(geoExtractor, geoLocationNormalizer)
  }

  def getGeoPhraseIDs(odinMentions: Seq[Mention]): Array[GeoPhraseID]= {
    val allOdinMentions = OdinMention.findAllByIdentity(odinMentions)
    val geoPhraseIDSeq: Seq[GeoPhraseID] = allOdinMentions.flatMap { odinMention =>
      odinMention.attachments.collect {
        case attachment: Location => attachment.geoPhraseID
      }
    }
    val geoPhraseIDMap: IdentityHashMap[GeoPhraseID, Int] = geoPhraseIDSeq.foldLeft(new IdentityHashMap[GeoPhraseID, Int]()) { (identityHashMap, geoPhraseID) =>
      identityHashMap.put(geoPhraseID, 0)
      identityHashMap
    }
    val geoPhraseIDArray = geoPhraseIDMap
        .keySet
        .asScala
        .toArray
        .sortWith { (left: GeoPhraseID, right: GeoPhraseID) =>
          if (left.startOffset != right.startOffset)
            left.startOffset < right.startOffset
          else if (left.endOffset != right.endOffset)
            left.endOffset < right.endOffset
          else
            true
        }

    geoPhraseIDArray
  }

  def getGeoPhraseIDs(odinMentions: Seq[Mention], sentences: Array[Sentence]): Array[Seq[GeoPhraseID]]= {
    val geoPhraseIDs: Seq[GeoPhraseID] = getGeoPhraseIDs(odinMentions)
    val alignedGeoPhraseIDs: Array[Seq[GeoPhraseID]] = sentences.map { sentence =>
      val sentenceStart = sentence.startOffsets.head
      val sentenceEnd = sentence.endOffsets.last

      geoPhraseIDs.filter { geoPhraseID =>
        sentenceStart <= geoPhraseID.startOffset && geoPhraseID.endOffset <= sentenceEnd
      }
    }
    alignedGeoPhraseIDs
  }
}

class GeoNormFinder(geoExtractor: GeoExtractor, normalizer: GeoLocationNormalizer) extends Finder {

  def getGeoPhraseIDs(odinMentions: Seq[Mention], sentences: Array[Sentence]): Array[Seq[GeoPhraseID]] =
      GeoNormFinder.getGeoPhraseIDs(odinMentions, sentences)

  def find(doc: Document, initialState: State): Seq[Mention] = {
    val Some(text) = doc.text

    // Make Location attachments from previously found Location mentions (e.g., from the gazetteer)
    val previouslyFound = initialState.allMentions.filter(_ matches "Location")
    val withAttachments = for {
        m <- previouslyFound
    } yield {
      val charStartIndex = m.startOffset
      val charEndIndex = m.endOffset
      val locationPhrase = m.text
      val geoID = normalizer(text, (charStartIndex, charEndIndex)).headOption.map {
        case (entry, _) => entry.id
      }
      val geoPhraseID = GeoPhraseID(locationPhrase, geoID, charStartIndex, charEndIndex)

      m.withAttachment(Location(geoPhraseID))
    }

    val sentenceLocations = geoExtractor.extract(doc.sentences.map(_.raw))

    val mentions = for {
      sentenceIndex <- doc.sentences.indices
      sentence = doc.sentences(sentenceIndex)
      locations = sentenceLocations(sentenceIndex)
      (wordStartIndex, wordEndIndex) <- locations
      charStartIndex = sentence.startOffsets(wordStartIndex)
      charEndIndex = sentence.endOffsets(wordEndIndex - 1)
      // There needs to be at least one character.  There may not be,
      // especially if the raw character doesn't really exist.
      // This happens if an implicit period completes a paragraph or document.
      locationPhrase = text.substring(charStartIndex, charEndIndex)
      if locationPhrase.length > 0
    } yield {
      val geoID = normalizer(text, (charStartIndex, charEndIndex)).headOption.map {
        case (entry, _) => entry.id
      }
      val geoPhraseID = GeoPhraseID(locationPhrase, geoID, charStartIndex, charEndIndex)

      new TextBoundMention(
        Seq("Location"),
        Interval(wordStartIndex, wordEndIndex),
        sentenceIndex,
        doc,
        true,
        getClass.getSimpleName,
        Set(Location(geoPhraseID))
      )
    }
    mentions ++ withAttachments
  }
}

