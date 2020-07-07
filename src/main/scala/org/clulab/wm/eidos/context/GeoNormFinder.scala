package org.clulab.wm.eidos.context

import java.nio.file.{Files, Path, Paths}
import java.util.IdentityHashMap
import org.clulab.dynet.Metal
import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.dynet.AnnotatedSentence
// import org.clulab.geonorm.{GeoLocationExtractor, GeoLocationNormalizer, GeoNamesIndex}
import org.clulab.geonorm.{GeoLocationNormalizer, GeoNamesIndex}
import org.clulab.odin.{Mention, State, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.Location
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.utils.OdinMention

import org.clulab.wm.eidos.EidosEnglishProcessor

import scala.collection.JavaConverters._

@SerialVersionUID(1L)
case class GeoPhraseID(text: String, geonameID: Option[String], startOffset: Int, endOffset: Int)

object GeoNormFinder {

  def fromConfig(config: Config): GeoNormFinder = {
    val geoNamesDir: Path = Paths.get(config[String]("geoNamesDir")).toAbsolutePath.normalize
    val geoNamesIndex =
        if (Files.exists(geoNamesDir) && Files.list(geoNamesDir).count() > 0)
          new GeoNamesIndex(geoNamesDir)
        else
          GeoNamesIndex.fromClasspathJar(geoNamesDir)

    val modelFilenamePrefix = config[String]("geonorm.modelFilenamePrefix")

    new GeoNormFinder(
      // new GeoLocationExtractor(),
      new GeoLocationNormalizer(geoNamesIndex),
      Some(Metal(modelFilenamePrefix))
      //new Metal(0, config[String]("geonorm.modelFilenamePrefix"))
    )
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

class GeoNormFinder(normalizer: GeoLocationNormalizer, metal:Option[Metal] ) extends Finder {

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


    // val sentenceLocations = extractor(doc.sentences.map(_.raw))
    //
    //
    // Adding the Metal NER tagged locations here
    //
    //

    def newRecognizeNamedEntities(doc: Document): Unit = {
      doc.sentences.foreach { sentence =>
        val words = sentence.words
        // val space_separated_sentence = words.mkString(" ")
        val posTags = Some(sentence.tags.get.toIndexedSeq) // these are probably wrong
      val neTags = Some(sentence.norms.get.toIndexedSeq) // these are probably wrong
      val annotatedSentence = new AnnotatedSentence(words, posTags, neTags)
        val predictions = metal.predict(0, annotatedSentence)

        // convert word-level class predictions into span-level geoname predictions

        for ((words, wordPredictions) <- sentence.words zip predictions) yield {
          // trim off any predictions on padding words
          // val wordPredictions = paddedWordPredictions.take(words.length)
          for {
            (wordPrediction, wordIndex) <- wordPredictions.zipWithIndex

            // a start is either a B, or an I that is following an O
            if wordPrediction == "B_LOC" || (wordPrediction == "I_LOC" &&
              // an I at the beginning of a sentence is not considered to be a start,
              // since as of Jun 2019, such tags seemed to be mostly errors (non-locations)
              wordIndex != 0 && wordPredictions(wordIndex - 1) == "O_LOC")
          } yield {

            // the end index is the first B or O (i.e., non-I) following the start
            val end = wordPredictions.indices.indexWhere(wordPredictions(_) != "I_LOC", wordIndex + 1)
            val endIndex = if (end == -1) words.length else end

            // yield the token span
            (wordIndex, endIndex)
          }
        }




        sentence.entities = Some(predictions.toArray) // this is probably wrong
      }
    }


    val sentenceLocations = EidosEnglishProcessor.




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

