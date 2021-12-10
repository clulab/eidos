package org.clulab.wm.eidos.context

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.geonorm.{GeoLocationExtractor, GeoLocationNormalizer, GeoNamesIndex}
import org.clulab.odin.{Mention, State, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.Location
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.mentions.OdinMention
import org.clulab.wm.eidos.utils.Unordered
import org.clulab.wm.eidos.utils.Unordered.OrderingOrElseBy
import org.clulab.wm.eidoscommon.utils.IdentityHashMap
import org.clulab.wm.eidoscommon.utils.IdentityHashSet

import scala.collection.mutable
import java.nio.file.{Files, Path, Paths}

@SerialVersionUID(1L)
case class GeoPhraseID(text: String, geonameID: Option[String], startOffset: Int, endOffset: Int)

object GeoPhraseID {
  implicit val ordering = Unordered[GeoPhraseID]
      .orElseBy(_.startOffset)
      .orElseBy(_.endOffset)
      .orElseBy(_.hashCode)
}

object GeoNormFinder {

  def fromConfig(config: Config): GeoNormFinder = {
    val geoNamesDir: Path = Paths.get(config[String]("geoNamesDir")).toAbsolutePath.normalize
    val geoNamesIndex =
        if (Files.exists(geoNamesDir) && Files.list(geoNamesDir).count() > 0)
          new GeoNamesIndex(geoNamesDir)
        else
          GeoNamesIndex.fromClasspathJar(geoNamesDir)

    new GeoNormFinder(
      new GeoLocationExtractor(),
      new GeoLocationNormalizer(geoNamesIndex)
    )
  }

  def getGeoPhraseIDs(odinMentions: Seq[Mention]): Array[GeoPhraseID]= {
    val allOdinMentions = OdinMention.findAllByIdentity(odinMentions)
    val geoPhraseIDSeq: Seq[GeoPhraseID] = allOdinMentions.flatMap { odinMention =>
      odinMention.attachments.collect {
        case attachment: Location => attachment.geoPhraseID
      }
    }
    val geoPhraseIDArray = IdentityHashSet(geoPhraseIDSeq)
        .toArray
        .sorted

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

class GeoNormFinder(extractor: GeoLocationExtractor, normalizer: GeoLocationNormalizer) extends Finder {

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

    val sentenceLocations = extractor(doc.sentences.map(_.raw))
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

