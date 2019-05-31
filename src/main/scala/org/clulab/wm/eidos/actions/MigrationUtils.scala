package org.clulab.wm.eidos.actions

import org.clulab.wm.eidos.attachments
import org.clulab.odin.{EventMention, Mention, State, TextBoundMention}
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments.{EidosAttachment, Location}
import org.clulab.wm.eidos.context.GeoPhraseID
import org.clulab.wm.eidos.document.EidosDocument
import org.clulab.wm.eidos.EidosActions
import org.clulab.wm.eidos.serialization.json.{JLDAttachment, JLDSerializer}
import org.clulab.wm.eidos.utils
import org.json4s.JValue
import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin._
import org.clulab.struct.Interval


object MigrationUtils {


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

    orig match {
      case tb: TextBoundMention => throw new RuntimeException("Textbound mentions are incompatible with argument expansion")
      case rm: RelationMention => rm.copy(arguments = expandedArgs, tokenInterval = newTokenInterval, foundBy = copyFoundBy)
      case em: EventMention => em.copy(arguments = expandedArgs, tokenInterval = newTokenInterval, foundBy = copyFoundBy, paths = paths)
    }
  }

  def processMigrationEvents(mentions: Seq[Mention]): Seq[Mention] = {
    // partition to get the migration events
    val (migrationEvents, other) = mentions.partition(_ matches EidosSystem.MIGRATION_LABEL)

    // TODO: do
    val handled = for {
      m <- migrationEvents
      oldArg = m.arguments("moveTo").head


      geolocs = m.document.asInstanceOf[EidosDocument].geolocs
      location: Option[GeoPhraseID] = if (geolocs.isDefined) geolocs.get(m.sentence).find(_.startOffset == oldArg.startOffset) else None
//      val newArg = oldArg.withAttachment(new Location(location))if (location.nonEmpty)
      newArg = if (location.nonEmpty) oldArg.withAttachment(new Location(location.head)) else oldArg

      args = m.arguments
      newArgs = m.arguments ++ Map("moveTo" -> Seq(newArg))
      newMention = copyWithNewArgs(m, newArgs)
    } yield  newMention
    // todo: add attachments for Time and Location (start with overlapping already found NN ones, see action)
    // todo: backoff times and locations -- use the normalization apis
    // todo: combine times (timeStart/timeEnd)????
    // todo: aggregation of cross-sentence stuff?????????????



//    def applyLocationAttachment(ms: Seq[Mention], state: State): Seq[Mention] = {
//      for {
//        m <- ms
//        trigger = m.asInstanceOf[EventMention].trigger
//        theme = m.arguments("theme").head
//        geolocs = m.document.asInstanceOf[EidosDocument].geolocs
//        location: Option[GeoPhraseID] = if (geolocs.isDefined) geolocs.get(m.sentence).find(_.startOffset == trigger.startOffset) else None
//      } yield location match {
//        case None => theme
//        case Some(l) => theme.withAttachment(new Location(l))
//      }
//    }


    // return all
    handled ++ other
  }

  def djasdklj(): Seq[Mention] = ???
}
