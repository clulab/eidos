package org.clulab.wm.eidos.context

import java.time.temporal.ChronoField.MONTH_OF_YEAR

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.timenorm.scate.Between
import org.clulab.timenorm.scate.RepeatingField
import org.clulab.timenorm.scate.ThisRI
import org.clulab.timenorm.scate.Year
import org.clulab.odin.{Mention, State, TextBoundMention}
import org.clulab.processors.{Document, Sentence}
import org.clulab.struct.{Interval => TextInterval}
import org.clulab.wm.eidos.attachments.{Location, Time}
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidoscommon.utils.FileUtils.getTextFromResource
import org.yaml.snakeyaml.Yaml

import scala.collection.JavaConverters._
import scala.math.max

case class SeasonMention(firstToken: Int, lastToken: Int, season: Map[String, Int], timeInterval: TimeStep)

object SeasonFinder {

  // SeasonFinder needs two data structures:
  //    - A Set of triggers that are used to find possible season mentions, e.g. "season", "meher"
  //    - A Map (key = GeoNameID) of Maps (key = season type) of Maps (key = star month & end month). For example,
  //      the planting season in Afar starts in June and ends in September:
  //          * "444179" -> "planting" -> ("start" -> 6, "end" -> 9)
  def fromConfig(config: Config): SeasonFinder = {
    val yaml = new Yaml()
    val seasonDBPath: String = config[String]("seasonsDBPath")
    val seasonDBTxt = getTextFromResource(seasonDBPath)
    val yamlDB = yaml.loadAll(seasonDBTxt).iterator()
    val seasonModifiersJavaMap = yamlDB.next().asInstanceOf[java.util.Map[String, Int]]
    val seasonModifiersMap = seasonModifiersJavaMap.asScala.toMap
    val seasonTriggersJavaList = yamlDB.next().asInstanceOf[java.util.Map[String, java.util.ArrayList[String]]]
    val seasonTriggersSet = seasonTriggersJavaList.get("triggers").asScala.toSet
    val seasonJavaMap = yamlDB.next().asInstanceOf[java.util.Map[String, java.util.Map[String, java.util.Map[String, Int]]]]
    val seasonMap = seasonJavaMap.asScala.mapValues(_.asScala.mapValues(_.asScala.toMap).toMap).toMap
    new SeasonFinder(seasonMap, seasonTriggersSet, seasonModifiersMap)
  }
}


class SeasonFinder(seasonMap: Map[String, Map[String, Map[String, Int]]], triggers: Set[String], modifierMap: Map[String, Int]) extends Finder{

  private def timeFilter(mention: Mention): Boolean = {
    mention.label == "Time" && (mention.attachments.head match {
      case attachment: Time => attachment.interval.intervals.nonEmpty
      case _ => false
    })
  }

  private def geoFilter(mention: Mention): Boolean = {
    mention.label == "Location" && (mention.attachments.head match {
      case attachment: Location => attachment.geoPhraseID.geonameID.exists(seasonMap.contains)
      case _ => false
    })
  }

  // This function is used to filter out mentions with no Time or Location attachments.
  private def filterMentions(distanceAndMention: (Int, Mention)): Boolean = {
    val mention = distanceAndMention._2

    // The two filters below can assume mention.attachments.head.
    mention.attachments.nonEmpty && (timeFilter(mention) || geoFilter(mention))
  }

  private def createMention(seasonMatch: (String, Int), sentIdx: Int, lemmas: Array[String], initialState: State): Option[SeasonMention] = {
    val seasonTrigger = seasonMatch._1
    val seasonTokenIdx = seasonMatch._2
    val sentSize = lemmas.length

    // Take all the mentions before and after the season trigger in the same sentence and all the mentions
    // in the previous 5 sentences. Sort them by their proximity to the trigger prioritizing same sentence:
    //    - Mentions in same sentence: Distance in tokens to the trigger
    //    - Mentions in previous sentences: Just their index in the reverse list + trigger's sentence length
    val previousMentions = initialState.mentionsFor(sentIdx, 0 until seasonTokenIdx).map(m => (seasonTokenIdx - m.tokenInterval.start, m))
    val followingMentions = initialState.mentionsFor(sentIdx, seasonTokenIdx + 1 until sentSize).map(m => (m.tokenInterval.start - seasonTokenIdx, m))
    val previousSentencesMentions = (max(0, sentIdx - 5) until sentIdx).reverse.flatMap(initialState.mentionsFor).zipWithIndex.map(m => (sentSize + m._2, m._1))
    val sortedMentions = (previousMentions ++ followingMentions ++ previousSentencesMentions).filter(filterMentions).sortBy(_._1).map(_._2)

    // Find and get the closest normalized geonameID.
    val geonameIDOpt: Option[String] = sortedMentions.find(geoFilter)
        .map(_.attachments.head.asInstanceOf[Location].geoPhraseID.geonameID.get)
    // Find and get the closest timeStep.
    val timeStepOpt: Option[TimeStep] = sortedMentions.find(timeFilter)
        .map(_.attachments.head.asInstanceOf[Time].interval.intervals.head)

    // If we find both Location and Time normalized create a SeasonMention if the season type is in the
    // seasons Map for that Location. SeasonMention is created with the first and last tokens
    // of the mention, the starting and ending months of the season according to the seasons Map, and
    // the timeInterval of Time that will be used as reference to normalize the season mention.
    // The season type if checked with following priority:
    //    1- The trigger is also the type (Uni-gram season expression), e.g. "meher"
    //    2- Long format type (Tri-gram season expression), e.g. "short rainy season"
    //    3- Short format type (Bi-gram season expression), e.g. "rainy season"
    if (geonameIDOpt.isDefined && timeStepOpt.isDefined) {
      val season = seasonMap(geonameIDOpt.get)
      val timeStep = timeStepOpt.get
      seasonTrigger match {
        case trigger if season.contains(trigger) =>
          Some(SeasonMention(seasonTokenIdx, seasonTokenIdx, season(trigger), timeStep))
        case _ if seasonTokenIdx > 0 =>
          val seasonType = lemmas(seasonTokenIdx - 1)
          lemmas.lift(seasonTokenIdx - 2) match {
            case Some(lemma) if season.contains(lemma + " " + seasonType) =>
              val seasonLongType = lemma + " " + seasonType
              Some(SeasonMention(seasonTokenIdx - 2, seasonTokenIdx, season(seasonLongType), timeStep))
            case _ if season.contains(seasonType) =>
              Some(SeasonMention(seasonTokenIdx - 1, seasonTokenIdx, season(seasonType), timeStep))
            case _ => None
          }
        case _ => None
      }
    } else {
      None
    }
  }

  private def findModifier(sentence: Sentence, tokenIdx: Int): Option[Int] = {
    val incomingHeads = sentence.dependencies.get.incomingEdges(tokenIdx).map(_._1)
    // Look for patterns like "late <- amod <- season" or "late <- amod <- start -> season"
    val modifiers = for (
      (head, dependant, edge) <- sentence.dependencies.get.allEdges
      if edge.matches("a(dv)?mod")
      if head == tokenIdx || incomingHeads.contains(head)
      if modifierMap.contains(sentence.lemmas.get(dependant))
    ) yield {
      dependant
    }
    modifiers.headOption
  }

  def find(doc: Document, initialState: State): Seq[Mention] = {
    val Some(text) = doc.text // Document must have a text.
    // For every season trigger found in text, try to create a SeasonMention and if it is defined
    // normalize it and create a TimeAttachment.
    val mentions = for {
      (sentence, sentenceIndex) <- doc.sentences.zipWithIndex
      lemmas = sentence.lemmas.get.map(_.toLowerCase)
      m <- lemmas.zipWithIndex.filter(s => triggers.contains(s._1))
      seasonMentionOpt = createMention(m, sentenceIndex, lemmas, initialState)
      if seasonMentionOpt.isDefined
      seasonMention = seasonMentionOpt.get
     } yield {
      val seasonStartMonth = seasonMention.season("start")
      val seasonEndMonth = seasonMention.season("end")

      // Look for a modifier in the expression context, get the offset span and
      // the number of months (shiftValue) to add/subtract to/from the time interval
      val modifierTokenIdx = findModifier(sentence, seasonMention.lastToken)
      val (modifierShift, seasonStartOffset, seasonEndOffset) = modifierTokenIdx match {
        case Some(tokenIdx) =>
          val firstExtendedIdx = if (tokenIdx < seasonMention.firstToken) tokenIdx else seasonMention.firstToken
          val lastExtendedIdx = if (tokenIdx > seasonMention.lastToken) tokenIdx else seasonMention.lastToken
          val shiftValue = modifierMap(sentence.lemmas.get(tokenIdx))
          (shiftValue, sentence.startOffsets(firstExtendedIdx), sentence.endOffsets(lastExtendedIdx))
        case None =>
          (0, sentence.startOffsets(seasonMention.firstToken), sentence.endOffsets(seasonMention.lastToken))
      }

      // TextInterval for the TimEx
      val seasonTextInterval = TextInterval(seasonStartOffset, seasonEndOffset)
      val seasonText = text.substring(seasonStartOffset, seasonEndOffset)

      // Normalize the season using the Year of the Time mention found in closestMention as reference.
      // Create a TimEx for this season.
      val startYearInterval = Year(seasonMention.timeInterval.startDate.getYear)
      val endYearInterval = if (seasonEndMonth >= seasonStartMonth)
        Year(seasonMention.timeInterval.startDate.getYear)
      else
        Year(seasonMention.timeInterval.startDate.getYear + 1)
      val startInterval = ThisRI(startYearInterval, RepeatingField(MONTH_OF_YEAR, seasonStartMonth))
      val endInterval = ThisRI(endYearInterval, RepeatingField(MONTH_OF_YEAR, seasonEndMonth))
      val seasonInterval = Between(startInterval, endInterval, startIncluded = true, endIncluded = true)
      val seasonStart = seasonInterval.start.plusMonths(modifierShift)
      val seasonEnd = seasonInterval.end.plusMonths(modifierShift)
      val timeSteps: Seq[TimeStep] = Seq(TimeStep (seasonStart, seasonEnd))
      val attachment = TimEx(seasonTextInterval, timeSteps, seasonText)

      // TextInterval for the TextBoundMention
      val wordInterval = TextInterval(seasonMention.firstToken, seasonMention.lastToken + 1)
      // create the Mention for this season expression
      new TextBoundMention(
        Seq("Time"),
        wordInterval,
        sentenceIndex,
        doc,
        true,
        getClass.getSimpleName,
        Set(Time(attachment))
      )
    }
    mentions
  }
}