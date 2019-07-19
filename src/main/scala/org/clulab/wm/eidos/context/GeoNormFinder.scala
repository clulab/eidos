package org.clulab.wm.eidos.context

import java.net.URL
import java.nio.file.{Files, Path, Paths}

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.geonorm.{GeoNamesIndex,GeoLocationExtractor,GeoLocationNormalizer}
import org.clulab.odin.{Mention, State, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.Location
import org.clulab.wm.eidos.document.EidosDocument
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.utils.FileUtils
import org.slf4j.LoggerFactory

import scala.collection.mutable

object GeoNormFinder {

  private lazy val logger = LoggerFactory.getLogger(getClass)

  class CacheManager(config: Config) {
    lazy val geoNamesIndexPath: Path = Paths.get(config[String]("geoNamesIndexPath"))
    protected lazy val segmentsPath: Path = geoNamesIndexPath.resolve("segments_1")
    protected lazy val zipPath: Path = geoNamesIndexPath.resolve("geonames-index.zip")

    // The default is not to replace any files on a machine that is simply running Eidos.
    // This can be overruled by programs that are managing the cache.
    def mkCache(replaceOnUnzip: Boolean = false): Unit = {
      // copy the zip file to the local machine
      val geoNamesIndexURL: URL = config[URL]("geoNamesIndexURL")
      logger.info(s"Downloading the GeoNames index from $geoNamesIndexURL.")
      Files.createDirectories(geoNamesIndexPath)
      Files.copy(geoNamesIndexURL.openStream, zipPath)

      // unzip the zip file
      logger.info(s"Extracting the GeoNames index to $geoNamesIndexPath.")
      FileUtils.unzip(zipPath, geoNamesIndexPath, replace = replaceOnUnzip)
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
        logger.info(s"GeoNames index found at $geoNamesIndexPath.")
      else
        logger.info(s"No GeoNames index at $geoNamesIndexPath.")
      cached
    }
  }

  def fromConfig(config: Config): GeoNormFinder = {
    val cacheManager = new CacheManager(config)
    if (!cacheManager.isCached)
      cacheManager.mkCache()

    new GeoNormFinder(
      new GeoLocationExtractor(),
      new GeoLocationNormalizer(new GeoNamesIndex(cacheManager.geoNamesIndexPath)))
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
          getClass.getSimpleName,
          Set(Location(geoPhraseID))
        )
      }
  }
}

