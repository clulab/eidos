package org.clulab.wm.eidos.utils

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin._
import org.clulab.wm.eidos.actions.CorefHandler
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidoscommon.EidosParameters
import org.clulab.wm.eidoscommon.utils.{FileUtils, StopwordManaging, TagSet}



class StopwordManager(stopwordsPath: String, transparentPath: String, tagSet: TagSet) extends StopwordManaging {
  protected val stopwords: Set[String] = FileUtils.getCommentedTextSetFromResource(stopwordsPath)
  protected def transparentWords: Set[String] = FileUtils.getCommentedTextSetFromResource(transparentPath)

  protected val bothWords: Set[String] = stopwords ++ transparentWords

  def containsStopword(stopword: String): Boolean = bothWords.contains(stopword)
  override def containsStopwordStrict(stopword: String): Boolean = stopwords.contains(stopword)

  def hasContent(mention: Mention, state: State): Boolean = hasContent(mention) || resolvedCoref(mention, state)

  def hasContent(mention: Mention): Boolean = {
    val lemmas = mention.lemmas.get
    val tags = mention.tags.get
    val entities = mention.entities.get

    // There should be at least one noun
    if (!tags.exists(tagSet.isAnyNoun)) return false
    // This above can be combined with those below

    // There should be at least one word which:
    lemmas.indices.exists { i =>
      // has more than one character
      lemmas(i).length > 1 &&
          // has a content POS tag
          tagSet.isStopwordContent(tags(i)) &&
          // isn't a stopword
          !containsStopword(lemmas(i)) &&
          // and isn't a stop NER
          !EidosParameters.STOP_NER.contains(entities(i))
    }
  }

  def resolvedCoref(mention: Mention, state: State): Boolean = {
    if (CorefHandler.hasCorefToResolve(mention)) {
      val corefRelations = state.allMentions.filter(m => m.matches(EidosParameters.COREF_LABEL)) // fixme
      corefRelations.exists(cr => cr.arguments.values.toSeq.flatten.contains(mention))
    }
    else false
  }

  def filterStopTransparent(mentions: Seq[Mention]): Seq[Mention] =
  // Remove mentions which are entirely stop/transparent words
    mentions.filter(hasContent)


  def keepCAGRelevant(mentions: Seq[Mention]): Seq[Mention] = {

    // 1) These will be "Causal" and "Correlation" which fall under "Event" if they have content
    val allMentions = State(mentions)
    val cagEdgeMentions = mentions.filter(m => releventEdge(m, allMentions))

    // Should these be included as well?

    // 3) These last ones may overlap with the above or include mentions not in the original list.
    val cagEdgeArguments = cagEdgeMentions.flatMap(mention => mention.arguments.values.flatten.toSeq)
    // Put them all together.
    // val releventEdgesAndTheirArgs = cagEdgeMentions ++ cagEdgeArguments
    // To preserve order, avoid repeats, and not allow anything new in the list, filter the original.
    mentions.filter(mention => isCAGRelevant(mention, cagEdgeMentions, cagEdgeArguments))
  }

  def isCAGRelevant(mention: Mention, cagEdgeMentions: Seq[Mention], cagEdgeArguments: Seq[Mention]): Boolean =
  // We're no longer keeping all modified entities
  //(mention.matches("Entity") && mention.attachments.nonEmpty) ||
    cagEdgeMentions.contains(mention) ||
        cagEdgeArguments.contains(mention)

  def relevantMentions(ad: AnnotatedDocument): Seq[EidosMention] = {
    val allMentions = ad.odinMentions
    ad.eidosMentions.filter(m => releventEdge(m.odinMention, State(allMentions)))
  }

  def releventEdge(m: Mention, state: State): Boolean = {
    m match {
      case tb: TextBoundMention => EidosParameters.EXPAND.contains(tb.label)
      case rm: RelationMention => EidosParameters.EXPAND.contains(rm.label)
      case em: EventMention => EidosParameters.EXPAND.contains(em.label) && argumentsHaveContent(em, state)
      case cs: CrossSentenceMention => EidosParameters.EXPAND.contains(cs.label)
      case _ => throw new UnsupportedClassVersionError()
    }
  }

  def argumentsHaveContent(mention: EventMention, state: State): Boolean = {
    val causes: Seq[Mention] = mention.arguments.getOrElse("cause", Seq.empty)
    val effects: Seq[Mention] = mention.arguments.getOrElse("effect", Seq.empty)

    if (causes.nonEmpty && effects.nonEmpty) // If it's something interesting,
    // then both causes and effects should have some content
    causes.exists(hasContent(_, state)) && effects.exists(hasContent(_, state))
    else
    true
  }
}

object StopwordManager {

  // maybe use this to get missed Locations/Dates/etc?; not sure if necessary anymore?
  //  val STOP_NER: Set[String] = Set("DURATION", "MONEY", "NUMBER", "ORDINAL", "ORGANIZATION", "PERCENT", "SET")


  def apply(stopwordsPath: String, transparentPath: String, tagSet: TagSet) =
      new StopwordManager(stopwordsPath, transparentPath, tagSet)

  def fromConfig(config: Config, tagSet: TagSet) = {
    val stopwordsPath: String = config[String]("filtering.stopWordsPath")
    val transparentPath: String = config[String]("filtering.transparentPath")
    apply(stopwordsPath, transparentPath, tagSet)
  }
}
