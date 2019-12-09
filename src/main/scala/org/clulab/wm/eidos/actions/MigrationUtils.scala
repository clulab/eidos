package org.clulab.wm.eidos.actions

import java.util.regex.Pattern

import org.clulab.odin.Attachment
import org.clulab.odin.{CrossSentenceMention, EventMention, Mention, RelationMention, TextBoundMention}
import org.clulab.wm.eidos.{EidosActions, EidosSystem}
import org.clulab.wm.eidos.mentions.CrossSentenceEventMention
import org.clulab._

import scala.annotation.tailrec

object MigrationUtils {

  def processMigrationEvents(mentions: Seq[Mention]): Seq[Mention] = {
    // partition to get the migration events
    val (migrationEvents, other) = mentions.partition(_ matches EidosSystem.MIGRATION_LABEL)

    // todo: backoff times and locations -- use the normalization apis
    // todo: combine times (timeStart/timeEnd)
    resolveGenericLocation(assembleFragmentsNew(migrationEvents)) ++ other
  }

  // combine events with shared arguments AND combine events in close proximity with complementary arguments
  def assembleFragmentsNew(mentions: Seq[Mention]): Seq[Mention] = {

    def findMerges(mentions: Seq[Mention]): (Array[Boolean], IndexedSeq[(Int, Int)]) = {
      val used = Array.fill(mentions.length)(false)
      val merges = mentions.indices.map { loIndex =>
        if (!used(loIndex)) {
          val mergeable = mentions
              .indices
              .drop(loIndex + 1)
              .filter { hiIndex =>
                isMergeable(mentions(loIndex), mentions(hiIndex))
              }

          mergeable.map { hiIndex =>
            used(loIndex) = true
            used(hiIndex) = true
            (loIndex, hiIndex)
          }
        }
        else
          Seq.empty
      }
      (used, merges.flatten)
    }

    def merge(mentions: Seq[Mention]): (Boolean, Seq[Mention]) = {
      val (used, merges) = findMerges(mentions)

      if (merges.nonEmpty) {
        val unorderedMergedMentionsOpt = merges.map { case (loIndex, hiIndex)  =>
          // println("Merging " + loIndex + " and " + hiIndex)
          val newArgs = mergeArgs(mentions(loIndex), mentions(hiIndex))
          val copy = copyWithNewArgs(mentions(hiIndex), newArgs)

          copy
        }
        val unorderedUnmergedMentionsOpt = used
            .indices
            .filter { hiIndex: Int => !used(hiIndex) }
            .map { hiIndex =>
              // println("Copying " + hiIndex)
              mentions(hiIndex)
            }
        // The merged ones are checked for equality, the unmerged not.
        val unorderedMergedMentions = unorderedMergedMentionsOpt.distinct ++ unorderedUnmergedMentionsOpt

        (false, unorderedMergedMentions)
      }
      else
        (true, mentions)
    }

    @tailrec
    def doWhile(mentions: Seq[Mention]): Seq[Mention] = {
      // This is the hack around a "do {} while (!condition)" in which the condition can't make use
      // of variables defined in the do block and the return value cannot be updated without a var.
      val (done, newMentions) = merge(mentions)

      if (done) newMentions
      else doWhile(newMentions)
    }

    doWhile(orderMentions(mentions))
  }

/*
 This is preserved temporarily to show what the new version above was aiming for.

  // combine events with shared arguments AND combine events in close proximity with complementary arguments
  def assembleFragmentsOld(mentions: Seq[Mention]): Seq[Mention] = {
    var orderedMentions = orderMentions(mentions)
    // the events we will ultimately return
    var returnedEvents = ArrayBuffer[Mention]()
    // keep merging events until we have nothing acceptable left to merge
    var stillMerging = true
    // loop and merge compatible events, add to mergedEvents
    while (stillMerging) {
      // empty the array at the beginning of each loop
      returnedEvents = ArrayBuffer[Mention]()
      // to keep track of what events we've merged
      var used = Array.fill(orderedMentions.length)(false)
      for (i <- orderedMentions.indices) {

        // only merge events if the first of the pair hasn't already been merged (heuristic!)
        if (!used(i)) {
          for (j <- i + 1 until orderedMentions.length) {
println("Checking " + i + " with " + j)
            //check if the two events can be merged
            if (isMergeable(orderedMentions(i), orderedMentions(j))) {
println("Merging " + i + " and " + j)
              // create the set of arguments to include in the new merged event (preference to the more specific args
              // in case of argName overlap)
              val newArgs = mergeArgs(orderedMentions(i), orderedMentions(j))
              //create a new event with the new args by copying the rightmost mention of the two with the new set of args;
              // copy the rightmost event and not the first one because this way we keep the possibility of this newly-merged
              // event being merged with a fragment from the next sentence in the next merging loop (currently, we only
              // merge fragments from adjacent sentences)
              val copy = copyWithNewArgs(orderedMentions(j), newArgs)
              // return the new event if it isn't identical to an existing event
              if (!(returnedEvents contains copy)) {
                returnedEvents += copy
                println("It did not contain.")
              }
              used = used.updated(i, true)
              used = used.updated(j, true)
            }
          }
        }
      }

      // add unmerged events ('false' in used list)
      // TODO Doesn't adding them now would mean that they are now out of order?
      for (i <- orderedMentions.indices) {
        if (!used(i)) {
          returnedEvents += orderedMentions(i)
println("Copying " + i)
        }
      }

      //check if there are any mergeable events among the newly-created set of mentions; if not, set stillMerging to false,
      // which will break the loop
      if (!returnedEvents.exists(mention => returnedEvents.exists(mention2 => isMergeable(mention, mention2) && mention != mention2 ))) {
        stillMerging = false
      }
      orderedMentions = returnedEvents
    }
    returnedEvents
  }
*/
  // given two event mentions, checks if they can be merged
  def isMergeable(mention1: Mention, mention2: Mention): Boolean = {
    // Don't construct the entire intersection just to find out whether or not it would be empty,
    // at least for small collections when this is repeatedly done.
    def intersects[T](left: Seq[T], right: Seq[T]): Boolean = left.exists(right.contains)

    // This will have to be calculated either way.
    val notBothSpecific = !bothSpecific(mention1, mention2)

    (
      // if the two events are within one sentence of each other
      Math.abs(mention1.sentence - mention2.sentence) <= 1 &&
      (
        // both events have complementary arguments (no intersection)
        // OR NOT both args with overlapping argName are specific (i.e., don't merge if both mentions have some specific/key
        // information with the same argName---merging will delete one of them); we want these to be separate events
        notBothSpecific || !intersects(mention1.arguments.keys.toSeq, mention2.arguments.keys.toSeq)
      )
    ) ||
    (
      // if both events share an argument
      // AND other arguments don't overlap (size of value intersection != size of key intersection) //todo: it does not look like we need this condition (it results in false negs at least in some cases), but keeping it here for now for potential future use
      // && m1.arguments.keys.toList.intersect(m2.arguments.keys.toList).size != m1.arguments.values.toList.intersect(m2.arguments.values.toList).size
      // AND NOT both args with overlapping argName are specific (i.e., don't merge if both mentions have some specific/key
      // information with the same argName---merging will delete one of them); we want these to be separate events
      notBothSpecific && intersects(mention1.arguments.values.toSeq, mention2.arguments.values.toSeq)
    )
  }

  val bothPattern: Pattern = Pattern.compile(".*[\\dA-Z]+.*")

  /*
  checks if both of the overlapping args are specific (AND are not the same arg because if they are the same argument,
  their...`specificity status` will be the same)
   */
  def bothSpecific(mention1: Mention, mention2: Mention): Boolean = {
    val overlappingArgNames = mention1.arguments.keys.toList.intersect(mention2.arguments.keys.toList)

    overlappingArgNames.exists { argName =>
      // It is assumed that the size is always one.
      val arg1 = mention1.arguments(argName).head
      val arg2 = mention2.arguments(argName).head

      // Specific events either have attachements or have some numeric information in them
      // (e.g., 300 refugees) or have capital letters in them
      (arg1.attachments.nonEmpty || bothPattern.matcher(arg1.text).matches) &&
      (arg2.attachments.nonEmpty || bothPattern.matcher(arg2.text).matches) &&
      // AND are not the same mention
      arg1 != arg2 &&
      // AND the arguments in question don't overlap.
      arg1.tokenInterval.intersect(arg2.tokenInterval).isEmpty
    }
  }

  // returns mentions in the order they appear in the document (based on sent index and tokenInterval of the mention)
  //todo: may go to mention utils
  def orderMentions(mentions: Seq[Mention]): Seq[Mention] = {
    mentions.sortWith { (left: Mention, right: Mention) =>
      if (left.sentence != right.sentence)
        left.sentence < right.sentence
      else if (left.tokenInterval != right.tokenInterval)
        left.tokenInterval < right.tokenInterval
      else
        true // left before right
    }
  }

  val mergePattern: Pattern = Pattern.compile(".*[A-Z\\d+].*")

  // merges args of two mentions in such a way as to hopefully return the more specific arg in case of an overlap
  // If the two are equally good, favor the first.
  def mergeArgs(mention1: Mention, mention2: Mention): Map[String, Seq[Mention]] = {
    val unionKeys = mention1.arguments.keys.toList.union(mention2.arguments.keys.toList)
    val newArgs = unionKeys.map { key =>
      val mentionsOpt1 = mention1.arguments.get(key)
      val mentionsOpt2 = mention2.arguments.get(key)
      // The actual arg will be from mention2 which would have overwritten that in mention1
      // If the argumentName is present in both of the mentions...
      // Choose the more specific argument by checking if one of them contains an attachment or contains numbers or contains capital letters
      val value = if (mentionsOpt1.isDefined && mentionsOpt2.isDefined) {
        val mentions1 = mentionsOpt1.get
        val mentions2 = mentionsOpt2.get
        val isGood1 = mentions1.exists(mention => mention.attachments.nonEmpty || mergePattern.matcher(mention.text).matches)
        val isGood2 = mentions2.exists(mention => mention.attachments.nonEmpty || mergePattern.matcher(mention.text).matches)

        if (isGood1) mentions1 // Favor first if it's good
        else if (isGood2) mentions2 // otherwise choose the second if that's good
        else mentions1 // and favor the first if neither is good.
      }
      else
        mentionsOpt1.getOrElse(mentionsOpt2.get) // There was only one of them anyway.

      key -> value
    }.toMap

    newArgs
  }

  /*
  if there is a generic location mention in the migration event, try to resolve it to the nearest previous specific location
  (for now, specific == has an attachment)
   */
  def resolveGenericLocation(mentions: Seq[Mention]): Seq[Mention] = {
    val orderedMentions = orderMentions(mentions)
    val resolvedMentions = orderedMentions.zipWithIndex.map { case (mention, index) =>
      val argNameOpt = getGenericLocArgName(mention)
      val geoLocMentionOpt = argNameOpt.flatMap { argName => findPrevGeoloc(orderedMentions, argName, index) }
      val newMentionOpt = geoLocMentionOpt.map { geoLocMention =>
        val argName = argNameOpt.get
        //create a corefMention between the current generic location (neighbor) and the specific one (anchor)
        val corefMention = new CrossSentenceMention(
          labels = Seq("Coreference"),
          anchor = geoLocMention,
          neighbor = mention.arguments(argName).head,
          arguments = Map[String, Seq[Mention]](
            (EidosActions.ANTECEDENT, Seq(geoLocMention)),
            (EidosActions.ANAPHOR, Seq(mention.arguments(argName).head))
          ),
          document = mention.document,
          keep = true,
          foundBy = s"resolveGenericLocationAction",
          attachments = Set.empty[Attachment]
        )

        //the new mention will be the copy of the current mention with the arguments including the newly-created
        //corefMention instead of the original generic location (the name of the arg is the same the generic location had)
        copyWithNewArgs(mention, mention.arguments ++ Map(argName -> Seq(corefMention)))
      }

      newMentionOpt.getOrElse(mention)
    }

    resolvedMentions
  }

  /*
  Given an ordered seq of mentions, the relevant argName, and the index of the current mention in the ordered seq,
  finds the nearest previous event that contains a specific location argument with the same argName (for now, specific ==
  has an attachment)
   */
  def findPrevGeoloc(orderedMentions: Seq[Mention], argName: String, order: Int): Option[Mention] = {
    val mentionOpt = orderedMentions.slice(0, order + 1).reverse.find { mention =>
      val argumentsOpt = mention.arguments.get(argName)
      val nonEmptyOpt = argumentsOpt.map(_.head.attachments.nonEmpty)

      nonEmptyOpt.getOrElse(false)
    }

    mentionOpt.map(_.arguments(argName).head)
  }

  // given a mention, returns the argName of the argument that contains a generic location
  def getGenericLocArgName(mention: Mention): Option[String] = {
    //look through the args; if arg contains a generic location, return the name of that arg
    val argumentOpt: Option[(String, Seq[Mention])] = mention.arguments.find(containsGenericLocation)

    argumentOpt.map(_._1)
  }

  // todo: revise the list
  val genericLocations: Seq[String] = Seq("country", "countries", "area", "areas", "camp", "camps", "settlement", "site")

  // given a complete argument (argName -> Mention), checks if it has an argument that is a generic location; todo: revise the list
  def containsGenericLocation(arg: (String, Seq[Mention])): Boolean =
      arg._2.exists(mention => genericLocations.contains(mention.text))

  protected def newWithinSentenceMention(mention: Mention, newArgs: Map[String, Seq[Mention]], foundByAffix: Option[String], mkNewInterval: Boolean): Mention = {
    val copyFoundBy = if (foundByAffix.nonEmpty) s"${mention.foundBy}_${foundByAffix.get}" else mention.foundBy
    val newTokenInterval = {
      if (mkNewInterval) {
        // All involved token intervals, both for the original event and the expanded arguments => changed to just
        // looking at the newArgs bc that involves the original set of args; may need to revisit
        // The arguments are all in the same sentence, so odin's mkTokenInterval should be valid.
        mention match {
          case mention: EventMention => odin.mkTokenInterval(mention.trigger, newArgs)
          case _: Mention => odin.mkTokenInterval(newArgs)
        }
      }
      else
        mention.tokenInterval
    }
    // Make the copy based on the type of the Mention
    val newMention = mention match {
      case rm: RelationMention => rm.copy(arguments = newArgs, tokenInterval = newTokenInterval, foundBy = copyFoundBy)
      case em: EventMention =>
        val paths = for {
          (argName, argPathsMap) <- mention.paths
          origPath = argPathsMap(mention.arguments(argName).head)
        } yield (argName, Map(newArgs(argName).head -> origPath))

        em.copy(arguments = newArgs, tokenInterval = newTokenInterval, foundBy = copyFoundBy, paths = paths)
      case _: TextBoundMention => throw new RuntimeException("Textbound mentions are incompatible with argument expansion")
    }

    newMention
  }

  //todo: place elsewhere --> mention utils
  //todo: is it generalizeable enough?
  def copyWithNewArgs(mention: Mention, newArgs: Map[String, Seq[Mention]], foundByAffix: Option[String] = None, mkNewInterval: Boolean = true): Mention = {
    // Create a mention to return as either another EventMention but with expanded args (the 'else' part) or a
    // crossSentenceEventMention if the args of the Event are from different sentences.
    val needsCrossSentenceForArgs = newArgs.values.exists { mentions: Seq[Mention] => mentions.exists(_.sentence != mention.sentence) }
    val needsCrossSentenceForTrigger = mention match {
      case mention: EventMention => mention.trigger.sentence != mention.sentence
      case _: Mention => false
    }
    val needsCrossSentence = needsCrossSentenceForArgs || needsCrossSentenceForTrigger
    val newMention = if (needsCrossSentence) {
      val trigger = mention.asInstanceOf[EventMention].trigger // This conversion doesn't seem to be a given!

      new CrossSentenceEventMention(labels = mention.labels, trigger,
          arguments = newArgs, Map.empty, mention.sentence, mention.document, keep = true,
          foundBy = mention.foundBy + "++ crossSentActions", attachments = mention.attachments)
    }
    else
      newWithinSentenceMention(mention, newArgs, foundByAffix, mkNewInterval)

    newMention
  }
}
