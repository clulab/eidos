package org.clulab.wm.eidos.context

import java.net.URL
import java.nio.file.{Files, Path, Paths}
import java.util.IdentityHashMap

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.geonorm.{GeoLocationExtractor, GeoLocationNormalizer, GeoNamesIndex}
import org.clulab.odin.{Mention, State, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.Location
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.FileUtils
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._

@SerialVersionUID(1L)
case class GeoPhraseID(text: String, geonameID: Option[String], startOffset: Int, endOffset: Int)

object GeoNormFinder {

  private lazy val logger = LoggerFactory.getLogger(getClass)

  class CacheManager(config: Config) {
    val geoNamesIndexDir: Path = Paths.get(config[String]("geoNamesIndexDir")).toAbsolutePath.normalize
    protected lazy val segmentsPath: Path = geoNamesIndexDir.resolve("segments_1")
    protected lazy val zipPath: Path = geoNamesIndexDir.resolve("geonames+woredas.zip")

    // The default is not to replace any files on a machine that is simply running Eidos.
    // This can be overruled by programs that are managing the cache.
    def mkCache(replaceOnUnzip: Boolean = false): Unit = {
      // copy the zip file to the local machine
      val geoNamesIndexURL: URL = config[URL]("geoNamesIndexURL")
      logger.info(s"Downloading the GeoNames index from $geoNamesIndexURL.")
      Files.createDirectories(geoNamesIndexDir)
      Files.copy(geoNamesIndexURL.openStream, zipPath)

      // unzip the zip file
      logger.info(s"Extracting the GeoNames index to $geoNamesIndexDir.")
      FileUtils.unzip(zipPath, geoNamesIndexDir, replace = replaceOnUnzip)
      Files.delete(zipPath)

      if (!isCached)
        throw new RuntimeException(s"The caching operation was apparently unsuccessful.")
    }

    def rmCache(): Unit = {
      Files.deleteIfExists(segmentsPath)
      Files.deleteIfExists(zipPath)
    }

    def isCached: Boolean = {
      val cached = Files.exists(segmentsPath)

      if (cached)
        logger.info(s"GeoNames index found at $geoNamesIndexDir.")
      else
        logger.info(s"No GeoNames index at $geoNamesIndexDir.")
      cached
    }
  }

  def fromConfig(config: Config): GeoNormFinder = {
    val cacheManager = new CacheManager(config)
    if (!cacheManager.isCached)
      cacheManager.mkCache()

    new GeoNormFinder(
      new GeoLocationExtractor(),
      new GeoLocationNormalizer(new GeoNamesIndex(cacheManager.geoNamesIndexDir)))
  }

  def getGeoPhraseIDs(odinMentions: Seq[Mention]): Array[GeoPhraseID]= {
    val reachableMentions = EidosMention.findReachableOdinMentions(odinMentions)
    val geoPhraseIDSeq: Seq[GeoPhraseID] = reachableMentions.flatMap { odinMention =>
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
      if (locationPhrase.length > 0)
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

