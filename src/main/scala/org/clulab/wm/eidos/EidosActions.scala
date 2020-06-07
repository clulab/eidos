package org.clulab.wm.eidos

import java.util.regex.Matcher
import java.util.regex.Pattern

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import org.clulab.odin._
import org.clulab.odin.EventMention
import org.clulab.processors.Sentence
import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.actions.MigrationHandler
import org.clulab.wm.eidos.attachments.CountModifier._
import org.clulab.wm.eidos.attachments.CountUnit._
import org.clulab.wm.eidos.expansion.Expander
import org.clulab.wm.eidos.expansion.MostCompleteEventsKeeper
import org.clulab.wm.eidos.utils.FoundBy
import org.clulab.wm.eidos.utils.MentionUtils
import org.clulab.wm.eidos.utils.TagSet
import org.slf4j.Logger
import org.slf4j.LoggerFactory


import scala.util.Try

// 1) the signature for an action `(mentions: Seq[Mention], state: State): Seq[Mention]`
// 2) the methods available on the `State`

//TODO: need to add polarity flipping

class EidosActions(val expansionHandler: Option[Expander]) extends Actions with LazyLogging {
  type Provenance = (String, Int, Int) // text, startOffset, endOffset
  type CountAndProvenance = (Double, Provenance)
  type CountUnitAndProvenanceOpt = (CountUnit.Value, Option[Provenance])
  type CountModifierAndProvenanceOpt = (CountModifier.Value, Option[Provenance])
  type NumberArg = (Int, Int, Double) // start, end, value

  val mostCompleteEventsKeeper = new MostCompleteEventsKeeper()

  /*
      Global Action -- performed after each round in Odin
  */
  def globalAction(mentions: Seq[Mention], state: State): Seq[Mention] = {
    // TODO: We can add this in if it makes things better/faster (i.e., not expanding duplicates)
    // val deduped = mentions.groupBy(mostCompleteEventsKeeper.weakIdentity).map(_._2.head).toVector
    // Expand mentions, if enabled
    val expanded = expansionHandler.map(_.expand(mentions, state)).getOrElse(mentions)

    // Stitch together causal chains
    val (causal, nonCausal) = expanded.partition(m => EidosSystem.CAG_EDGES.contains(m.label))
    val assembled = createEventChain(causal, "effect", "cause")

    // TODO: in the sentence below we stitch together the sequence of cause->effect events
    //  but some expanded nounphrase remains, which shouldn't be displayed in the webapp
    //    In Kenya , the shortened length of the main growing season , due in part to a delayed onset of seasonal rainfall , coupled with long dry spells and below-average rainfall is resulting in below-average production prospects in large parts of the eastern , central , and southern Rift Valley .

    assembled ++ nonCausal
  }

  protected def getCount(groupArg: Mention, numberArg: NumberArg): CountAndProvenance = {
    (
      numberArg._3,
      (
        groupArg.sentenceObj.words.slice(numberArg._1, numberArg._2).mkString(" "),
        groupArg.sentenceObj.startOffsets(numberArg._1),
        groupArg.sentenceObj.endOffsets(numberArg._2 - 1)
      )
    )
  }

  protected def getProvenanceOpt(pattern: Pattern, text: String, offset: Int): Option[Provenance] = {
    val matcher: Matcher = pattern.matcher(text)

    if (matcher.find) {
      val matchedText = text.slice(matcher.start, matcher.end)

      Some((matchedText, matcher.start + offset, matcher.end + offset))
    }
    else None
  }

  protected def getCountUnit(groupArg: Mention, numberArg: NumberArg, text: String, offset: Int): CountUnitAndProvenanceOpt = {
    if (groupArg.sentenceObj.entities.get(numberArg._1) == "PERCENT")
      (Percentage, Some((groupArg.sentenceObj.words(numberArg._1), groupArg.sentenceObj.startOffsets(numberArg._1), groupArg.sentenceObj.endOffsets(numberArg._1))))
    else {
      def getCountUnitOpt(countUnit: CountUnit, pattern: Pattern): Option[CountUnitAndProvenanceOpt] = {
        val provenanceOpt = getProvenanceOpt(pattern, text, offset)

        provenanceOpt.map { provenance => (countUnit, Some(provenance)) }
      }

      None
          .orElse(getCountUnitOpt(Daily, EidosActions.DAILY_PATTERN))
          .orElse(getCountUnitOpt(Weekly, EidosActions.WEEKLY_PATTERN))
          .orElse(getCountUnitOpt(Monthly, EidosActions.MONTHLY_PATTERN))
          .getOrElse((Absolute, None))
    }
  }

  protected def getCountModifier(groupArg: Mention, numberArg: NumberArg, text: String, offset: Int): CountModifierAndProvenanceOpt = {

    def getCountModifierOpt(countModifier: CountModifier, pattern: Pattern): Option[CountModifierAndProvenanceOpt] = {
      val provenanceOpt = getProvenanceOpt(pattern, text, offset)

      provenanceOpt.map { provenance => (countModifier, Some(provenance)) }
    }

    None
        .orElse(getCountModifierOpt(Approximate, EidosActions.APPROXIMATE_PATTERN))
        .orElse(getCountModifierOpt(Min, EidosActions.MIN_PATTERN))
        .orElse(getCountModifierOpt(Max, EidosActions.MAX_PATTERN))
        .getOrElse((NoModifier, None))
  }

  protected def newCountAttachment(count: CountAndProvenance, countModifier: CountModifierAndProvenanceOpt, countUnit: CountUnitAndProvenanceOpt): CountAttachment = {

    def isPrefix(value: String, texts: Seq[String]): Boolean = texts.exists { text =>
      text.length > value.length && text.startsWith(value)
    }

    // This is an attempt to make the text pretty and describe the interval succinctly.
    val option: Seq[Option[Provenance]] = Seq(Some(count._2), countModifier._2, countUnit._2)
    val some: Seq[Provenance] = option.filter(_.isDefined).map(_.get)
    val sortedByStartOffset: Seq[Provenance] = some.sortBy(_._2)
    val texts: Seq[String] = sortedByStartOffset.map(_._1)
    // Percents in particular cause repeated values like "about 75 percent 75"
    // Keep 75 percent, but rule out 75.
    val longTexts: Seq[String] = texts.filter { text => !isPrefix(text, texts) }
    val text: String = longTexts.mkString(" ")
    val startOffset: Int = sortedByStartOffset.head._2
    val sortedByEndOffset: Seq[Provenance] = some.sortBy(_._3)
    val endOffset: Int = sortedByEndOffset.last._3

    new CountAttachment(text, MigrationGroupCount(count._1, countModifier._1, countUnit._1), startOffset, endOffset)
  }

  protected def getCountAttachmentOpt(mention: Mention): Option[EidosAttachment] = {
    if (mention.isInstanceOf[EventMention] && mention.label == "HumanMigration" && mention.arguments.contains("group")) {
      val groupArg = mention.arguments("group").head // there should be a single group; this should be safe
      val numberArgOpt: Option[NumberArg] = extractNumber(groupArg.sentenceObj, groupArg.start, groupArg.end)

      numberArgOpt.map { numberArg =>
        val startWordIndex = math.max(0, mention.start - 1)
        val endWordIndex = math.min(mention.end + 1, mention.sentenceObj.size)
        val eventText = mention.sentenceObj.words.slice(startWordIndex, endWordIndex).mkString(" ")
        val eventStartOffset = mention.sentenceObj.startOffsets(startWordIndex)
        val count = getCount(groupArg, numberArg)
        val countUnit = getCountUnit(groupArg, numberArg, eventText, eventStartOffset)
        val countModifier = getCountModifier(groupArg, numberArg, eventText, eventStartOffset)
        val countAttachment = newCountAttachment(count, countModifier, countUnit)

        countAttachment
      }.orElse(None)
    }
    else
      None
  }

  /**
    * Normalizes migration events, e.g., by extracting the actual number of people displaced from the "group" argument
    * @param mentions Sequence of mentions to be normalized
    * @param state unused
    * @return the sequence of normalized mentions
    */
  def normalizeGroup(mentions: Seq[Mention], state: State): Seq[Mention] = {
    mentions.map { mention =>
      val countAttachmentOpt = getCountAttachmentOpt(mention)

      countAttachmentOpt.map { countAttachment =>
        val oldGroupArg = mention.arguments("group").head //getting the group arg from the original event
        val newGroupArg = oldGroupArg.withAttachment(countAttachment) //copying the old one but with the newly-found attachments
        val newArgs = mention.arguments ++ Map("group" -> Seq(newGroupArg)) //creating the new set of args by taking the original event arguments and adding the new group argument
        val withNewArg = MigrationHandler.copyWithNewArgs(mention, newArgs, Some("withGroupNormalized")) //creating the new event argument with the new set of arguments

        withNewArg
      }.getOrElse(mention)
    }
  }

  def migrationActionFlow(mentions: Seq[Mention], state: State): Seq[Mention] =
      withArgs(normalizeGroup(mentions, state))

  def withArgs(mentions: Seq[Mention]): Seq[Mention] =
      mentions.filter(_.arguments.nonEmpty)

  protected def extractNumber(sentence: Sentence, startGroup: Int, endGroup: Int): Option[(Int, Int, Double)] = {
    // we need the NER tags for number extraction; these are better than regexes!
    if (sentence.entities.isEmpty)
      return None

    var start = -1
    var end = -1
    var done = false

    for(i <- startGroup until endGroup if ! done) {
      val ne = sentence.entities.get(i)
      if(ne == "NUMBER" || ne == "PERCENT") {
        if(start == -1) {
          start = i
        }
        end = i + 1
      } else if(start != -1) {
        done = true
      }
    }

    if (start != -1) {
      // ideally, get the value of this number from .norms
      // however, this fails sometimes now, because the B/I-TIME tags override this column!
      // let's try to be robust; fall back to words if needed
      val n = toNumber(sentence.norms.get(start))
      if (n.isDefined)
        return Some((start, end, n.get))
      else if (end == start + 1) {
        val n = toNumber(sentence.words(start))
        if (n.isDefined)
          return Some((start, end, n.get))
      }
    }

    // could not find a meaningful number
    None
  }

  protected def toNumber(string: String): Option[Double] = {
    val matcher = EidosActions.BAD_NUMBER_PATTERN.matcher(string)
    val cleanString = matcher.replaceAll("")

    Try(cleanString.toDouble).toOption
  }

  def createEventChain(causal: Seq[Mention], arg1: String, arg2: String): Seq[Mention] = {
    val arg1Mentions = State(causal.flatMap(_.arguments.getOrElse(arg1, Nil)))
    // replace event causes with captured effects if possible
    // to stitch together sequences of causal events
    val assembled = for {
      m <- causal
      _ = require(m.arguments(arg2).length == 1, "we only support a single cause and effect per event")
      arg2Mention <- m.arguments.getOrElse(arg2, Nil)
    } yield {
      if (m.paths.isEmpty) {
        // if the mention was captured by a surface rule then there is no syntactic path to retrieve
        // return mention as-is
        Seq(m)
      } else {
        // odin mentions keep track of the path between the trigger and the argument
        // below we assume there is only one cause arg, so beware (see require statement above)
        val landed = m.paths(arg2)(m.arguments(arg2).head).last._2 // when the rule matched, it landed on this
        assembleEventChain(m.asInstanceOf[EventMention], arg2Mention, landed, arg1Mentions)
      }
    }

    assembled.flatten
  }

  /*
      Filtering Methods
   */

  def importanceFromLengthAndAttachments(m: EventMention): Int = {
    //val allArgMentions = m.arguments.values.toSeq.flatten.map(mention => mention.attachments.size)
    mostCompleteEventsKeeper.argTokenInterval(m).length
  }

  /*
      Method for assembling Event Chain Events from flat text, i.e., ("(A --> B)" --> C) ==> (A --> B), (B --> C)
  */

  // this method ensures that if a cause of one event is the effect of another then they are connected
  // FIXME -- not working for this:
  //    However , prospects for 2017 aggregate cereal production are generally unfavourable as agricultural activities continue to be severely affected by the protracted and widespread insecurity , which is constraining farmers ' access to fields and is causing large scale displacement of people , input shortages and damage to households ' productive assets .
  def assembleEventChain(event: EventMention, origCause: Mention, landed: Int, effects: State): Seq[EventMention] = {
    effects.mentionsFor(event.sentence, landed) match {
      case Seq() => Seq(event) // return original event
      case newCauses =>
        val arguments = event.arguments
        newCauses.map { nc =>
          val retainedCauses = arguments("cause").filterNot(_ == origCause)
          val newArguments = arguments.filterKeys(_ != "cause") + (("cause", retainedCauses :+ nc))
          val mentions = newArguments.values.flatten.toSeq :+ event.trigger
          val newStart = mentions.map(_.start).min
          val newEnd = mentions.map(_.end).max

          event.copy(arguments = newArguments, tokenInterval = Interval(newStart, newEnd))
        }
    }
  }

  /*
      Mention State / Attachment Methods
   */

  //Rule to apply quantifiers directly to the state of an Entity (e.g. "small puppies") and
  //Rule to add Increase/Decrease to the state of an entity
  //TODO: perhaps keep token interval of the EVENT because it will be longer?
  // todo BECKY: if trigger is also a quant (i.e. has a quant attachment), add as quant in inc/dec
  def applyAttachment(ms: Seq[Mention], state: State): Seq[Mention] = {
    for {
      m <- ms
      //if m matches "EntityModifier"
      attachment = getAttachment(m)

      copyWithMod = m match {
        case tb: TextBoundMention => tb.copy(attachments = tb.attachments ++ Set(attachment), foundBy = s"${tb.foundBy}++mod")
        // Here, we want to keep the theme that is being modified, not the modification event itself
        case rm: RelationMention =>
          val theme = mostCompleteEventsKeeper.tieBreaker(rm.arguments("theme")).asInstanceOf[TextBoundMention]
          theme.copy(attachments = theme.attachments ++ Set(attachment), foundBy = FoundBy(theme).add(rm))
        case em: EventMention =>
          val theme = mostCompleteEventsKeeper.tieBreaker(em.arguments("theme")).asInstanceOf[TextBoundMention]
          theme.copy(attachments = theme.attachments ++ Set(attachment), foundBy = FoundBy(theme).add(em))
      }
    } yield copyWithMod
  }

  def applyTimeAttachment(ms: Seq[Mention], state: State): Seq[Mention] = {
    for (m <- ms; entity <- m.arguments("entity"); time <- m.arguments("time")) yield {
      MentionUtils.withMoreAttachments(entity, time.attachments.toSeq)
    }
  }

  def applyLocationAttachment(ms: Seq[Mention], state: State): Seq[Mention] = {
    for (m <- ms; entity <- m.arguments("entity"); location <- m.arguments("location")) yield {
      MentionUtils.withMoreAttachments(entity, location.attachments.toSeq)
    }
  }

  def debug(ms: Seq[Mention], state: State): Seq[Mention] = {
    logger.debug("DEBUG ACTION")
    ms
  }

  def getAttachment(mention: Mention): EidosAttachment = EidosAttachment.newEidosAttachment(mention)

  def pass(mentions: Seq[Mention], state: State): Seq[Mention] = mentions


}

object EidosActions extends Actions {
  lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val BAD_NUMBER_PATTERN: Pattern = Pattern.compile("[~%,<>]")

  val DAILY_PATTERN: Pattern = Pattern.compile("daily")
  val WEEKLY_PATTERN: Pattern = Pattern.compile("weekly")
  val MONTHLY_PATTERN: Pattern = Pattern.compile("monthly")

  val APPROXIMATE_PATTERN: Pattern = Pattern.compile("""about|approximately|around""")
  val MIN_PATTERN: Pattern = Pattern.compile("""(at\s+least)|(more\s+than)|over""")
  val MAX_PATTERN: Pattern = Pattern.compile("""(at\s+most)|(less\s+than)|almost|under""")

  def fromConfig(config: Config, tagSet: TagSet): EidosActions = {

    val useExpansion: Boolean = config[Boolean]("useExpansion")
    val expansionHandler = if (useExpansion) Some(Expander.fromConfig(config[Config]("expander"), tagSet)) else None

    new EidosActions(expansionHandler)
  }
}
