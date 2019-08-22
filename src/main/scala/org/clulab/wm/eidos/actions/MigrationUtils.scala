package org.clulab.wm.eidos.actions

import org.clulab.odin._
import org.clulab.struct.{CorefMention, Interval}
import org.clulab.odin.{EventMention, Mention, State, TextBoundMention}
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments.{Location, Time}
import org.clulab.wm.eidos.context.GeoPhraseID
import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin._
import org.clulab.odin.impl.Taxonomy
import org.clulab.processors.Document
import org.clulab.wm.eidos.{EidosActions, EidosSystem}
import org.clulab.wm.eidos.EidosActions.COREF_DETERMINERS
import org.clulab.wm.eidos.mentions.CrossSentenceEventMention
import org.clulab.wm.eidos.utils.{DisplayUtils, FileUtils}
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object MigrationUtils {

  def processMigrationEvents(mentions: Seq[Mention]): Seq[Mention] = {
    // partition to get the migration events
    val (migrationEvents, other) = mentions.partition(_ matches EidosSystem.MIGRATION_LABEL)
//    val handled = attachGeoLoc(migrationEvents)
    // todo: backoff times and locations -- use the normalization apis
    // todo: combine times (timeStart/timeEnd)????

    // return all
//        handled ++ other
//    assembleFragments(handled) ++ other
//    assembleMoreSpecific(assembleFragments(handled)) ++ other
//    assembleTime(assembleFragments(handled)) ++ other

//    resolveGenericLocation(assembleTime(assembleFragments(handled))) ++ other
    resolveGenericLocation(assembleFragments(migrationEvents)) ++ other
//    migrationEvents ++ other
  }

  def assembleFragments(mentions: Seq[Mention]): Seq[Mention] = {
    // combine events with shared arguments AND combine events in close proximity with complementary arguments

    var orderedMentions = orderMentions(mentions)

    // the events we will ultimately return
    var returnedEvents = ArrayBuffer[Mention]()

    // keep merging events until we have nothing acceptable left to merge
    var stillMerging = true


    // loop and merge compatible events, add to mergedEvents
    while (stillMerging) {
//      println("\nLOOPING")

      // empty the array at the beginning of each loop
      returnedEvents = ArrayBuffer[Mention]()

      // to keep track of what events we've merged
      var used = Array.fill(orderedMentions.length)(false)


      for (i <- orderedMentions.indices) {
//        println("i-th mention-->" + i + " " + orderedMentions(i).text)

        // only merge events if the first of the pair hasn't already been merged (heuristic!)
        if (!used(i)) {
          for (j <- i+1 until orderedMentions.length) {
//            println("j-th mention--->" + j + " " + orderedMentions(j).text)

            //check if the two events can be merged
            if (isMergeable(orderedMentions(i), orderedMentions(j))) {

              // create the set of arguments to include in the new merged event (preference to the more specific args
              // in case of argName overlap)
              val newArgs = mergeArgs(orderedMentions(i), orderedMentions(j))
//              println("ORIG MENTION: " + orderedMentions(i).text)

              //create a new event with the new args by copying the rightmost mention of the two with the new set of args;
              // copy the rightmost event and not the first one because this way we keep the possibility of this newly-merged
              // event being merged with a fragment from the next sentence in the next merging loop (currently, we only
              // merge fragments from adjacent sentences)
              val copy = copyWithNewArgs(orderedMentions(j), newArgs)

              // return the new event if it isn't identical to an existing event
              if (!(returnedEvents contains copy)) {
                returnedEvents += copy
//                println("merged: " + orderedMentions(i).text + " AND " + orderedMentions(j).text + "\n" + "Resuling event: " + copy.text + "|")
              }
              used = used.updated(i,true)
              used = used.updated(j,true)
            }
          }
        }

      }

      // add unmerged events ('false' in used list)
      for (i <- orderedMentions.indices) {
        if (!used(i)) {
          returnedEvents += orderedMentions(i)
        }
      }

      //check if there are any mergeable events among the newly-created set of mentions; if not, set stillMerging to false,
      // which will break the loop
      if (!returnedEvents.exists(mention => returnedEvents.exists(mention2 => isMergeable(mention, mention2) && mention!= mention2 ))) {
        stillMerging = false
      }
      orderedMentions = returnedEvents
//      for (e <- returnedEvents) println("returned after loop: " + e.text)
//      println("end of loop")
    }

    returnedEvents

  }


  /*
  given two event mentions, checks if they can be merged
   */
  def isMergeable(m1: Mention, m2: Mention): Boolean = {

    // if the two events are within one sentence of each other
    if ((Math.abs(m1.sentence - m2.sentence) < 2
      // AND both events have complementary arguments (no overlap)
      && m1.arguments.keys.toList.intersect(m2.arguments.keys.toList).isEmpty)
    // OR
    ||
    // if both events share an argument
    (m1.arguments.values.toList.intersect(m2.arguments.values.toList).nonEmpty
      // AND other arguments don't overlap (size of value intersection != size of key intersection) //todo: it does not look like we need this condition (it results in false negs at least in some cases), but keeping it here for now for potential future use
      // && m1.arguments.keys.toList.intersect(m2.arguments.keys.toList).size != m1.arguments.values.toList.intersect(m2.arguments.values.toList).size

      //AND NOT both args with overlapping argName are specific (i.e., don't merge if both mentions have some specific/key
      //information with the same argName---merging will delete one of them); we want these to be separate events
      && !bothSpecific(m1, m2)
      )
    //OR
    ||
    //if within one sent of each other
    (Math.abs(m1.sentence - m2.sentence) < 2

      //AND events share the type of argument (argName)
      && (m1.arguments.keys.toList.intersect(m2.arguments.keys.toList).nonEmpty

      //AND NOT both args with overlapping argName are specific (i.e., don't merge if both mentions have some specific/key
      //information with the same argName---merging will delete one of them); we want these to be separate events
      && !bothSpecific(m1, m2))

      )) return true

    false
  }


  /*
  checks if both of the overlapping args are specific (AND are not the same arg because if they are the same argument,
  their...`specificity status` will be the same)
   */
  def bothSpecific(m1: Mention, m2: Mention): Boolean = {
    val overlappingArgNames = m1.arguments.keys.toList.intersect(m2.arguments.keys.toList)
    for (argName <- overlappingArgNames) {
      val relArg1 = m1.arguments(argName).head
      val relArg2 = m2.arguments(argName).head

      //specific events either have attachements or have some numeric information in them (e.g., 300 refugees)
      //AND are not the same mention
      if ((relArg1.attachments.nonEmpty || (relArg1.text matches ".*\\d+.*")) && (relArg2.attachments.nonEmpty || (relArg2.text matches ".*\\d+.*")) && relArg1 != relArg2  ) return true
    }

    false
  }


  /*
  returns mentions in the order they appear in the document (based on sent index and tokenInterval of the mention)
   */
  //todo: may go to mention utils
  def orderMentions(mentions: Seq[Mention]): Seq[Mention] = {
    val grouped = mentions.groupBy(_.sentence)

    //contains all mentions ordered by sent and by order in the sent
    var mentionArray = ArrayBuffer[Mention]()

    //for every sentence (sorted)...
    for (i <- grouped.keys.toList.sorted) {
      //...sort the mentions and append them to the mention array
      val sorted = grouped(i).sortBy(_.tokenInterval)
      for (m <- sorted) mentionArray.append(m)
    }
    mentionArray
  }


  /*
  merges args of two mentions in such a way as to hopefully return the more specific arg in case of an overlap
   */
  //keep in migr utils

  def mergeArgs(mention1: Mention, mention2: Mention): Map[String, Seq[Mention]] = {
    var newArgs = scala.collection.mutable.Map[String, Seq[Mention]]()
    //overlapping argument names
    val overlappingArgs = mention1.arguments.keys.toList.intersect(mention2.arguments.keys.toList)
    //for every argument in the two mentions...
    for (arg <- mention1.arguments ++ mention2.arguments) {
      //if the argumentName is present in both of the mentions...
      if (overlappingArgs.contains(arg._1)) {
        //choose the more specific argument by checking if one of them contains an attachment or contains numbers
        val arg1 = mention1.arguments(arg._1)
        if (arg1.exists(tbm => tbm.attachments.nonEmpty || (tbm.text matches ".*\\d+.*"))) {
          newArgs = newArgs ++ Map(arg._1 -> arg1)
        } else {
          newArgs = newArgs ++ Map(arg._1 -> mention2.arguments(arg._1))
        }

      } else {
        newArgs = newArgs ++ Map(arg)
      }
    }

    newArgs.toMap
  }


  /*
  if there is a generic location mention in the migration event, try to resolve it to the nearest previous specific location
  (for now, specific == has an attachment)
   */
  def resolveGenericLocation(mentions: Seq[Mention]): Seq[Mention] = {
    val orderedMentions = orderMentions(mentions)
    val handled = ArrayBuffer[Mention]()
    for (m <- orderedMentions) {
      //check if the mention has a generic location argument
      if (containsGenericLocation(m)) {

        //get the name of the argument that contains the generic location
        val argName = getGenericLocArgName(m)
        //find the index of the current mention (used to look for specific locations only in the previous events)
        val indexInOrderedMentions = orderedMentions.indexOf(m)
        //check if there exists a previous event with a specific location
        if (hasPrevGeoloc(orderedMentions, argName, indexInOrderedMentions)) {
          //find previous specific location
          val geoLocMention = findPrevGeoloc(orderedMentions, argName, indexInOrderedMentions)
          //create a corefMention between the current generic location (neighbor) and the specific one (anchor)
          val corefMention = new CrossSentenceMention(
            labels = Seq("corefMention"),
            anchor = geoLocMention,
            neighbor = m.arguments(argName).head,
            arguments = Map[String, Seq[Mention]]((EidosActions.ANTECEDENT, Seq(geoLocMention)), (EidosActions.ANAPHOR, Seq(m.arguments(argName).head))),
            document = m.document,
            keep = true,
            foundBy = s"resolveGenericLocationAction",
            attachments = Set.empty[Attachment]
          )

          //the new mention added to `handled` will be the copy of the current mention with the arguments including the newly-created
          //corefMention instead of the original generic location (the name of the arg is the same the generic location had)
          handled += copyWithNewArgs(m, m.arguments ++ Map(argName -> Seq(corefMention)))

        } else {
          handled += m
        }

      } else {
        handled += m
      }
    }
      handled
    }


  /*
  Given an ordered seq of mentions, the relevant argName, and the index of the current mention in the ordered seq,
  checks if there exists a previous event that contains a specific location argument with the same argName (for now, specific ==
  has an attachment)
   */
  def hasPrevGeoloc(mentions: Seq[Mention], argName: String, order: Int): Boolean = {
    for (m <- mentions.slice(0, order + 1)) {
      if (m.arguments.keys.toList.contains(argName)) {
        if (m.arguments(argName).head.attachments.nonEmpty) {
          return true
        }
      }
    }
    false

  }


  /*
  Given an ordered seq of mentions, the relevant argName, and the index of the current mention in the ordered seq,
  finds the nearest previous event that contains a specific location argument with the same argName (for now, specific ==
  has an attachment)
   */
  def findPrevGeoloc(orderedMentions: Seq[Mention], argName: String, order: Int): Mention = {

    var relevantMentions = ArrayBuffer[Mention]()
    while (relevantMentions.isEmpty) {
      for (m <- orderedMentions.slice(0, order + 1).reverse) {
        if (m.arguments.keys.toList.contains(argName)) {
          if (m.arguments(argName).head.attachments.nonEmpty) {
            relevantMentions += m.arguments(argName).head
          }
        }
      }
    }

    relevantMentions.head

  }


  /*
  given a mention, returns the argName of the argument that contains a generic location
   */
  def getGenericLocArgName(m: Mention): String = {
    //look through the args; if arg contains a generic location, return the name of that arg
    var stringArray = ArrayBuffer[String]()

    while (stringArray.isEmpty) {
      for (arg <- m.arguments) {
        if (containsGenericLocation(arg)) {
          stringArray += arg._1
        }

      }
    }
    stringArray.head

  }


  /*
  given a mention, checks if it has an argument that is a generic location; todo: revise the list
   */
  def containsGenericLocation(m: Mention): Boolean = {
    val genericLocations = Seq("country", "countries", "area", "areas", "camp", "camps", "settlement", "site")
    if (m.arguments.exists(arg => arg._2.exists(tbm => genericLocations.contains(tbm.text)))) {
      return true
    }
  false
  }


  /*
  given a complete argument (argName -> Mention), checks if it has an argument that is a generic location; todo: revise the list
   */
  def containsGenericLocation(arg: (String, Seq[Mention])): Boolean = {
    val genericLocations = Seq("country", "countries", "area", "areas", "camp", "camps", "settlement", "site")
    if (arg._2.exists(m =>  genericLocations.contains(m.text))) {
      return true
    }
    false
  }


  //todo: place elsewhere --> mention utils
  //todo: is it generalizeable enough?
  def copyWithNewArgs(orig: Mention, newArgs: Map[String, Seq[Mention]], foundByAffix: Option[String] = None, mkNewInterval: Boolean = true): Mention = {
    // Helper method to get a token interval for the new event mention with expanded args
    def getNewTokenInterval(intervals: Seq[Interval]): Interval = Interval(intervals.minBy(_.start).start, intervals.maxBy(_.end).end)

    val newTokenInterval = if (mkNewInterval) {
      // All involved token intervals, both for the original event and the expanded arguments
      val allIntervals = Seq(orig.tokenInterval) ++ newArgs.values.flatten.map(arg => arg.tokenInterval)
      // Find the largest span from these intervals
      getNewTokenInterval(allIntervals)
    }
    else orig.tokenInterval

    val paths = for {
      (argName, argPathsMap) <- orig.paths
      origPath = argPathsMap(orig.arguments(argName).head)
    } yield (argName, Map(newArgs(argName).head -> origPath))

    // Make the copy based on the type of the Mention
    val copyFoundBy = if (foundByAffix.nonEmpty) s"${orig.foundBy}_${foundByAffix.get}" else orig.foundBy

    val newArgsAsList = for {
      seqMen <- newArgs.values
      men <- seqMen
    } yield men

    //create a mention to return as either another EventMention but with expanded args (the 'else' part) or a crossSentenceEventMention if the args of the Event are from different sentences
    val newMention = if (newArgsAsList.exists(_.sentence != orig.sentence) ) {
      //      orig.asInstanceOf[EventMention].copy(arguments = newArgs, tokenInterval = newTokenInterval, foundBy = copyFoundBy, paths = Map.empty)
      new CrossSentenceEventMention(labels = orig.labels, tokenInterval = newTokenInterval, trigger = orig.asInstanceOf[EventMention].trigger, arguments = newArgs, Map.empty, orig.sentence, orig.document, keep = true, foundBy = orig.foundBy + "++ crossSentActions", attachments = orig.attachments)

    }else {

      orig match {
        case tb: TextBoundMention => throw new RuntimeException("Textbound mentions are incompatible with argument expansion")
        case rm: RelationMention => rm.copy(arguments = newArgs, tokenInterval = newTokenInterval, foundBy = copyFoundBy)
        case em: EventMention => em.copy(arguments = newArgs, tokenInterval = newTokenInterval, foundBy = copyFoundBy, paths = paths)
      }
    }

    newMention

  }

}

