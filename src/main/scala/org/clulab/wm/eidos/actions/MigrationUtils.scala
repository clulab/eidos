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
import org.clulab.wm.eidos.{EidosActions, EidosSystem}
import org.clulab.wm.eidos.EidosActions.{COREF_DETERMINERS}
import org.clulab.wm.eidos.utils.FileUtils
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

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
    noArgOverlap(assemblyForOverlappingMentions(handled)) ++ other
  }

  def assemblyForOverlappingMentions(mentions: Seq[Mention]): Seq[Mention] = {
    val groupedMentions = mentions.groupBy(_.sentence)
    val afterAssembly = for  {
      group <- groupedMentions
      e <- group._2
      e1 <- group._2
      if (e != e1 && e.arguments.values.toList.intersect(e1.arguments.values.toList).nonEmpty)

    } yield copyWithNewArgs(e, e1.arguments ++ e.arguments)

    val handled = if (afterAssembly.nonEmpty) mostCompleArg(afterAssembly.toSeq) else mentions
    handled
  }



  def mostCompleArg(mentions: Seq[Mention]): Seq[Mention] = {
    val sorted = mentions.sortBy(_.arguments.toList.length).reverse
    val mostArguments = sorted.head
    Seq(mostArguments)
  }

  def noArgOverlap(mentions: Seq[Mention]): Seq[Mention] = {

    val afterAssembly = for  {
      e <- mentions
      e1 <- mentions if (Math.abs(e.sentence - e1.sentence) < 2)
      if (e != e1 && e.arguments.values.toList.intersect(e1.arguments.values.toList).isEmpty) //&& Math.abs(e.sentence - e1.sentence) < 2 )

    } yield copyWithNewArgs(e, e1.arguments ++ e.arguments)

    val handled = if (afterAssembly.nonEmpty) mostCompleArg(afterAssembly.toSeq) else mentions
    handled
  }


//  def resolve(mentions: Seq[Mention], allMention: Seq[Mention]): Seq[Mention] = {
//    val allGrouped = allMention.groupBy(_.sentence)
//    val handled = for {
//      m <- mentions
//      newArgs = for {
//        arg <- m.arguments
//        if (arg._2.head.foundBy matches "simple-np")
//
//      } yield arg
//
//
//      if (newArgs.nonEmpty)
////      presentcSent = m.document.sentences(m.sentence - 1)
//
//      anaphor = newArgs.head._2.head
//      antecedent = allGrouped(m.sentence - 1).filter(_.foundBy matches "location-nn").head
//
//
////      corefMention = new CrossSentenceMention(
////        labels = Seq("coref"),
////        anchor = antecedent,
////        neighbor = anaphor,
////        arguments = Map[String, Seq[Mention]]((EidosActions.ANTECEDENT, Seq(antecedent)), (EidosActions.ANAPHOR, Seq(anaphor))),
////        document = m.document,
////        keep = true,
////        foundBy = s"coref",
////        attachments = Set.empty[Attachment]
////      )
//
//    } //yield copyWithNewArgs(m, m.arguments ++ Map("coref" -> Seq(corefMention)))
//
//    mentions ++ handled
//  }



//
//  def mostArgs(mentions: Seq[Mention]): Seq[Mention] = {
//    val sorted = mentions.sortBy(_.arguments.toList.length).reverse
//    var handled = List(sorted.head)
//    for (m <- sorted.tail) {
//      val mArgs = m.arguments.values.toList
//      for (m2 <- handled) {
//        val m2Args = m2.arguments.values.toList
//        if (m2Args contains mArgs) {
//          handled :+ m
//        }
//      }
//    }
//    handled
//  }


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

}

//attempting to not return one migratio event over the whole paragraph. mainly failed.
//def assemble(mentions: Seq[Mention]): Seq[Mention] = {
//  val groupedMentions = mentions.groupBy(_.sentence)
//  val afterAssembly = for {
//  group <- groupedMentions
//  allSentEvents = for {
//  e <- group._2
//  e1 <- group._2
//  if (e != e1 && e.arguments.values.toList.intersect(e1.arguments.values.toList).nonEmpty)
//  //      argIntersectAll = e.arguments.keys.toList.intersect(e1.arguments.keys.toList)
//  //      diffValuesArg = for {
//  //        argName <- argIntersectAll
//  //        if (e.arguments(argName)!=e1.arguments(argName))
//  //      } yield argName
//  //      overlapArgAsSeq = Map(diffValuesArg.head -> Seq(e.arguments(diffValuesArg.head).head, e1.arguments(diffValuesArg.head).head))
//
//} yield copyWithNewArgs(e, e1.arguments ++ e.arguments)
//
//} yield mostArgs(allSentEvents).head
//
//  val handled = if (afterAssembly.nonEmpty) afterAssembly.toSeq else mentions
//  handled
//}