package org.clulab.wm.eidos.context

import java.time.LocalDateTime
import java.time.temporal.ChronoField.MONTH_OF_YEAR

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.yaml.snakeyaml.Yaml
import org.clulab.timenorm.scate._
import org.clulab.odin.{Mention, State, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.struct.{Interval => TextInterval}
import org.clulab.wm.eidos.attachments.{Location => LocationAttachment, Time => TimeAttachment}
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.utils.FileUtils.getTextFromResource

import scala.math.max
import scala.collection.JavaConverters._


case class SeasonMention(firstToken: Int, secondToken: Int, season: Map[String, Int], timeInterval: TimeStep)

object SeasonFinder {

  def fromConfig(config: Config): SeasonFinder = {
    val seasonDBPath: String = config[String]("seasonsDBPath")
    val seasonDBTxt = getTextFromResource(seasonDBPath)
    val yaml = new Yaml()
    val seasonJavaMap = yaml.load(seasonDBTxt).asInstanceOf[java.util.Map[String, java.util.Map[String, java.util.Map[String, Int]]]]
    val seasonMap = seasonJavaMap.asScala.mapValues(_.asScala.mapValues(_.asScala.toMap).toMap).toMap
    new SeasonFinder(seasonMap)
  }
}


class SeasonFinder(seasonMap: Map[String, Map[String, Map[String, Int]]]) extends Finder{

  private def filterMentions(mention: (Int, Mention)): Boolean = {
    if (mention._2.attachments.isEmpty)
      false
    else
      mention._2.attachments.head match {
        case a: LocationAttachment if a.geoPhraseID.geonameID.isDefined => true
        case a: TimeAttachment if a.interval.intervals.nonEmpty => true
        case _ => false
      }
  }

  private def closestMention(seasonMatch: Seq[(String, Int)], sentIdx: Int, sentSize: Int, initialState: State): Option[SeasonMention] = {
    val firstTokenIdx = seasonMatch.head._2
    val secondTokenIdx = seasonMatch.last._2
    val seasonName = seasonMatch.head._1

    val previousMentions = initialState.mentionsFor(sentIdx, 0 to firstTokenIdx).map(m => (firstTokenIdx - m.tokenInterval.start, m))
    val followingMentions = initialState.mentionsFor(sentIdx, secondTokenIdx + 1 to sentSize).map(m => (m.tokenInterval.start - secondTokenIdx, m))
    val previousSentencesMentions = (max(0, sentIdx - 5) until sentIdx).reverse.flatMap(initialState.mentionsFor).zipWithIndex.map(m => (sentSize + m._2, m._1))
    val sortedMentions = (previousMentions ++ followingMentions ++ previousSentencesMentions).filter(filterMentions).sortBy(_._1).map(_._2)

    val geoLocation = sortedMentions.filter(_.label == "Location")
      .map(_.attachments.head).map(_.asInstanceOf[LocationAttachment])
      .find(m => seasonMap.contains(m.geoPhraseID.geonameID.get))

    val timeInterval = sortedMentions.filter(_.label == "Time")
      .map(_.attachments.head).map(_.asInstanceOf[TimeAttachment].interval.intervals.headOption)
      .find(_.isDefined)

    if (geoLocation.isDefined && timeInterval.isDefined) {
      val geoLocationID = geoLocation.get.geoPhraseID.geonameID.get
      if (seasonMap(geoLocationID).contains(seasonName)) {
        Some(SeasonMention(firstTokenIdx, secondTokenIdx, seasonMap(geoLocationID)(seasonName), timeInterval.get.head))
      } else {
        None
      }
    } else {
      None
    }
  }

  def find(doc: Document, initialState: State): Seq[Mention] = {
    val Some(text) = doc.text // Document must have a text.
    val mentions = for {(sentence, sentenceIndex) <- doc.sentences.zipWithIndex
                        m <- sentence.lemmas.get.zipWithIndex.iterator.sliding(2, 1).filter(_.last._1 == "season")
                        seasonMention = closestMention(m, sentenceIndex, sentence.size, initialState)
                        if seasonMention.isDefined
    } yield {
      val seasonStartOffset = sentence.startOffsets(seasonMention.get.firstToken)
      val seasonEndOffset = sentence.endOffsets(seasonMention.get.secondToken)
      val seasonStartMonth = seasonMention.get.season("start")
      val seasonEndMonth = seasonMention.get.season("end")

      val seasonTextInterval = TextInterval(seasonStartOffset, seasonEndOffset)
      val seasonText = text.substring(seasonStartOffset, seasonEndOffset)

      val yearInterval = Year(seasonMention.get.timeInterval.startDate.getYear)
      val startInterval = ThisRI(yearInterval, RepeatingField(MONTH_OF_YEAR, seasonStartMonth))
      val endInterval = ThisRI(yearInterval, RepeatingField(MONTH_OF_YEAR, seasonEndMonth))
      val seasonInterval = Between(startInterval, endInterval, startIncluded = true, endIncluded = true)
      val timeSteps: Seq[TimeStep] = Seq(TimeStep (seasonInterval.start, seasonInterval.end))
      val attachment = TimEx(seasonTextInterval, timeSteps, seasonText)

      val wordInterval = TextInterval(seasonMention.get.firstToken, seasonMention.get.secondToken + 1)
      //println(sentenceIndex, seasonInterval.start, seasonInterval.end, seasonName, seasonText, timexName, geoLocationName)
      // create the Mention for this time expression
      new TextBoundMention(
        Seq("Time"),
        wordInterval,
        sentenceIndex,
        doc,
        true,
        getClass.getSimpleName,
        Set(TimeAttachment(attachment))
      )
    }
    mentions
  }
}