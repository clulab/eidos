package org.clulab.wm.eidos.actions

import org.clulab.odin._
import org.clulab.struct.Interval
import org.clulab.odin.{EventMention, Mention, State, TextBoundMention}
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments.Location
import org.clulab.wm.eidos.context.GeoPhraseID
import org.clulab.wm.eidos.document.EidosDocument
import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin._
import org.clulab.odin.impl.Taxonomy
import org.clulab.processors.Document
import org.clulab.wm.eidos.{EidosActions, EidosSystem}
import org.clulab.wm.eidos.EidosActions.COREF_DETERMINERS
import org.clulab.wm.eidos.utils.{DisplayUtils, FileUtils}
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object MigrationUtils {

  def processMigrationEvents(mentions: Seq[Mention]): Seq[Mention] = {
    // partition to get the migration events
    val (migrationEvents, other) = mentions.partition(_ matches EidosSystem.MIGRATION_LABEL)
    val relArgs = Array("moveTo", "moveFrom", "moveThrough") //location-related args in migration events

    val handled = for {
      m <- migrationEvents
      geolocs = m.document.asInstanceOf[EidosDocument].geolocs
      oldArgs = for {
        arg <- relArgs
        if m.arguments.get(arg).nonEmpty
      } yield arg //name of args actually present in the mention

      //this should create args with geoloc attachments
      newArgs = for {
        oldArg <- oldArgs
        oldArgMention = m.arguments(oldArg).head
        location: Option[GeoPhraseID] = if (geolocs.isDefined) geolocs.get(m.sentence).find(_.startOffset == oldArgMention.startOffset) else None
        if location.nonEmpty
        newArg = oldArgMention.withAttachment(new Location(location.head))

      } yield Map(oldArg -> Seq(newArg))

      updatedArgs = m.arguments ++ newArgs.flatten.toMap //old arguments ++ the newly created args with attachments.

    } yield copyWithNewArgs(m, updatedArgs) //create a copy of the original event mention, but with the arguments that
    //also contain attachments
    // todo: backoff times and locations -- use the normalization apis
    // todo: combine times (timeStart/timeEnd)????
    // todo: aggregation of cross-sentence stuff?????????????

    // return all
    //    handled ++ other
    assembleMoreSpecific(assembleFragments(handled)) ++ other
  }


  def assembleFragments(mentions: Seq[Mention]): Seq[Mention] = {
    // combine events with shared arguments AND combine events in close proximity with complementary arguments

    // to keep track of what events we've merged
    var used = Array.fill(mentions.length)(false)

    // the events we will ultimately return
    var returnedEvents = Array[Mention]()

    // keep merging events until we have nothing acceptable left to merge
    var stillMerging = true

    // loop and merge compatible events, add to mergedEvents
    while (stillMerging) {
      for (i <- mentions.indices) {
        stillMerging = false
        // only merge events if the first of the pair hasn't already been merged (heuristic!)
        if (!used(i)) {
          for (j <- i+1 until mentions.length) {
            if (
            // the two events are within one sentence of each other
              (Math.abs(mentions(i).sentence - mentions(j).sentence) < 2
                // AND if both events have complementary arguments (no overlap)
                && mentions(i).arguments.keys.toList.intersect(mentions(j).arguments.keys.toList).isEmpty)
                // OR
                ||
                // if both events share an argument
                (mentions(i).arguments.values.toList.intersect(mentions(j).arguments.values.toList).nonEmpty
                  // AND other arguments don't overlap (size of value intersection != size of key intersection)
                  && mentions(i).arguments.keys.toList.intersect(mentions(j).arguments.keys.toList).size == mentions(i).arguments.values.toList.intersect(mentions(j).arguments.values.toList).size)
            ) {
              // merge the two events into one new event, keeping arguments from both
              val copy = copyWithNewArgs(mentions(i), mentions(i).arguments ++ mentions(j).arguments)
              stillMerging = true
              // return the new event if it isn't identical to an existing event
              if (!(returnedEvents contains copy)) {
                returnedEvents = returnedEvents :+ copy
              }
              used = used.updated(i,true)
              used = used.updated(j,true)
            }
          }
        }

      }
      for (e <- returnedEvents) println("returned after loop: " + e.text)
      println("end of loop")
    }

    // add unmerged events ('false' in used list)
    for (i <- mentions.indices) {
      if (!used(i)) {
        returnedEvents = returnedEvents :+ mentions(i)
      }
    }
    returnedEvents
  }

  /*
  current heuristic: if migration mention has a location (or todo: time), merge it with the previous migration mention (todo: merge only if it has the type of argument that we are missing an attachment for)
   */
  def assembleMoreSpecific(mentions: Seq[Mention]): Seq[Mention] = {
    //all human migration events grouped by sentence
    val grouped = mentions.groupBy(_.sentence)

    //all hme ordered by sent and by order in the sent
    var mentionArray = ArrayBuffer[Mention]()
    //for every sentence (sorted)...
    for (i <- grouped.keys.toList.sorted) {
      println("i: " + i)
      println("len: " + grouped.keys.toList.length)
      println("keys: " + grouped.keys.toList.sorted.mkString("|"))
      //...sort the mentions and append them to the mention array
      val sorted = grouped(i).sortBy(_.tokenInterval)
      for (m <- sorted) mentionArray.append(m)
    }

    //todo: keep track of what has already been merged

    var used = Array.fill(mentionArray.length)(false)

    //todo: still doesn't merge everything, e.g. "400 people left Sudan. 300 refugees fled South Sudan. They left the country for Ethiopia. They left in 1997. At the time, they had nowhere to go."

    for (m <- mentionArray) println("--->" + m.text + " " + m.start + " " + m.label)
    val needAttachment = for {
      m <- mentionArray //getting human migr events
      arg <- m.arguments //getting (argname --> Seq(tbms))
      tbm <- arg._2 //each tbm for each arg
      if (tbm matches "Location") //todo: do this for time, too
      if (tbm.attachments.isEmpty)
      newArgs = m.arguments ++ mentionArray(mentionArray.indexOf(m)-1).arguments //todo: "prev mention" doesn't work bc it can be an overlapping mention---need to filter them somehow


    } yield copyWithNewArgs(m, newArgs)

    for (m <- needAttachment) println("NEEDS ATTACHMENT: " + m.text)
    mentions ++ needAttachment

  }

  //todo: place elsewhere
  //todo: is it generalizeable enough?
  def copyWithNewArgs(orig: Mention, expandedArgs: Map[String, Seq[Mention]], foundByAffix: Option[String] = None, mkNewInterval: Boolean = true): Mention = {
    // Helper method to get a token interval for the new event mention with expanded args
    def getNewTokenInterval(intervals: Seq[Interval]): Interval = Interval(intervals.minBy(_.start).start, intervals.maxBy(_.end).end)

    val newTokenInterval = if (mkNewInterval) {
      // All involved token intervals, both for the original event and the expanded arguments
      val allIntervals = Seq(orig.tokenInterval) ++ expandedArgs.values.flatten.map(arg => arg.tokenInterval)
      // Find the largest span from these intervals
      getNewTokenInterval(allIntervals)
    }
    else orig.tokenInterval

    val paths = for {
      (argName, argPathsMap) <- orig.paths
      origPath = argPathsMap(orig.arguments(argName).head)
    } yield (argName, Map(expandedArgs(argName).head -> origPath))

    // Make the copy based on the type of the Mention
    val copyFoundBy = if (foundByAffix.nonEmpty) s"${orig.foundBy}_$foundByAffix" else orig.foundBy

    val newArgsAsList = for {
      seqMen <- expandedArgs.values
      men <- seqMen
    } yield men

    //create a mention to return as either another EventMention but with expanded args (the else part) or a crossSentenceEventMention if the args of the Event are from different sentences
    val newMention = if (newArgsAsList.exists(_.sentence != orig.sentence) ) {
      //      orig.asInstanceOf[EventMention].copy(arguments = expandedArgs, tokenInterval = newTokenInterval, foundBy = copyFoundBy, paths = Map.empty)
      new CrossSentenceEventMention(labels = orig.labels, tokenInterval = newTokenInterval, trigger = orig.asInstanceOf[EventMention].trigger, arguments = expandedArgs, Map.empty, orig.sentence, orig.document, keep = true, foundBy = orig.foundBy + "++ crossSentActions", attachments = Set.empty)

    }else {

      orig match {
        case tb: TextBoundMention => throw new RuntimeException("Textbound mentions are incompatible with argument expansion")
        case rm: RelationMention => rm.copy(arguments = expandedArgs, tokenInterval = newTokenInterval, foundBy = copyFoundBy)
        case em: EventMention => em.copy(arguments = expandedArgs, tokenInterval = newTokenInterval, foundBy = copyFoundBy, paths = paths)
      }
    }

    newMention

  }

}

//same as EventMention but with a different foundBy, no paths, sentence == the sent of the trigger, and a different text method
//todo: place elsewhere
//todo: change tokenInterval to something informed and usable
class CrossSentenceEventMention(
                                 override val labels: Seq[String],
                                 override val tokenInterval: Interval,
                                 override val trigger: TextBoundMention,
                                 override val arguments: Map[String, Seq[Mention]],
                                 override val paths: Map[String, Map[Mention, SynPath]],
                                 override val sentence: Int,
                                 override val document: Document,
                                 override val keep: Boolean,
                                 override val foundBy: String,
                                 override val attachments: Set[Attachment] = Set.empty
                               ) extends EventMention(labels, tokenInterval, trigger, arguments, Map.empty, trigger.sentence, document, keep, foundBy, attachments) {

  //the text method is overridden bc the EventMention text method does not work with cross sentence mentions
  //todo: is the text of a mention the arg span or the trigger span should also be included (in cases when all the args are to one side of the trigger)?
  override def text: String = {
    val argsBySent = arguments.map(a => a._2.toList).flatten.toList.groupBy(m => m.sentence)
    val textArray = new ArrayBuffer[String]()
    val lastSentIndex = argsBySent.keys.toList.length - 1


    for (i <- argsBySent.keys.toList.sorted) {
      //      println("i: " + i)
      //      println("len: " + argsBySent.keys.toList.length)
      val sortedMentions = argsBySent(i).sortBy(_.startOffset)
      val firstArgInd = sortedMentions.head.start
      val sentTextArr = argsBySent(i).head.sentenceObj.raw
      i match {
        case 0 => {
          val relSubString = sentTextArr.slice(firstArgInd, sentTextArr.length).mkString(" ")
          textArray.append(relSubString)
        }
        case `lastSentIndex` => {
          val relSubString = sentTextArr.slice(0, sortedMentions.last.end).mkString(" ")
          textArray.append(relSubString)
        }
        case _ => textArray.append(sentTextArr.mkString(" "))
      }
    }

    val text = textArray.mkString(" ")

    text
  }


}