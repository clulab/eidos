package org.clulab.wm.eidos.utils

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin._
import org.clulab.wm.eidos.{EidosActions, EidosSystem}

trait StopwordManaging {
  def containsStopword(stopword: String): Boolean
  def containsStopwordStrict(stopword: String): Boolean = containsStopword(stopword)
}

class StopwordManager(stopwordsPath: String, transparentPath: String) extends StopwordManaging {
  protected def stopwords: Set[String] = FileUtils.getCommentedTextSetFromResource(stopwordsPath)
  protected def transparentWords: Set[String] = FileUtils.getCommentedTextSetFromResource(transparentPath)

  protected val bothWords = stopwords ++ transparentWords

  def containsStopword(stopword: String): Boolean = bothWords.contains(stopword)

  def hasContent(mention: Mention, state: State): Boolean = hasContent(mention) || resolvedCoref(mention, state)

  def hasContent(mention: Mention): Boolean = {
    val lemmas = mention.lemmas.get
    val tags = mention.tags.get
    val entities = mention.entities.get

    //println(s"Checking mention: ${mention.text}")
    lemmas.indices.exists { i =>
      isContentPOS(tags(i)) &&
      tags(i) != "VBN" && // we don't want entities/concepts which consist ONLY of a VBN
      !containsStopword(lemmas(i)) &&
        !StopwordManager.STOP_POS.contains(tags(i)) &&
        !StopwordManager.STOP_NER.contains(entities(i))
    }
  }

  def resolvedCoref(mention: Mention, state: State): Boolean = {
    if (hasCorefToResolve(mention)) {
      val corefRelations = state.allMentions.filter(m => m.matches(EidosSystem.COREF_LABEL))
      corefRelations.exists(cr => cr.arguments.values.toSeq.flatten.contains(mention))
    }
    else false
  }

  def hasCorefToResolve(m: Mention): Boolean = {
    m match {
      case tb: TextBoundMention => EidosActions.startsWithCorefDeterminer(tb)
      case rm: RelationMention => EidosActions.existsDeterminerCause(rm)
      case em: EventMention => EidosActions.existsDeterminerCause(em)
      case _ => false
    }
  }

  def isContentPOS(tag: String): Boolean = StopwordManager.CONTENT_POS_PREFIXES.exists(prefix => tag.startsWith(prefix))


  def filterStopTransparent(mentions: Seq[Mention]): Seq[Mention] =
      // Remove mentions which are entirely stop/transparent words
      mentions.filter(hasContent)

  override def containsStopwordStrict(stopword: String): Boolean = stopwords.contains(stopword)

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

  def releventEdge(m: Mention, state: State): Boolean = {
    m match {
      case tb: TextBoundMention => EidosSystem.CAG_EDGES.contains(tb.label)
      case rm: RelationMention => EidosSystem.CAG_EDGES.contains(rm.label)
      case em: EventMention => EidosSystem.CAG_EDGES.contains(em.label) && argumentsHaveContent(em, state)
      case cs: CrossSentenceMention => EidosSystem.CAG_EDGES.contains(cs.label)
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
  val CONTENT_POS_PREFIXES: Set[String] = Set("ADJ", "NOUN", "NN", "PROPN", "VERB", "VB", "JJ")
  val STOP_POS: Set[String] = Set("CD")
  val STOP_NER: Set[String] = Set("DATE", "DURATION", "LOCATION", "MONEY", "NUMBER", "ORDINAL", "ORGANIZATION", "PERCENT", "PERSON", "PLACE", "SET", "TIME")

  def apply(stopwordsPath: String, transparentPath: String) = new StopwordManager(stopwordsPath, transparentPath)

  def fromConfig(config: Config) = {
    val stopwordsPath: String = config[String]("stopWordsPath")
    val transparentPath: String = config[String]("transparentPath")
    apply(stopwordsPath, transparentPath)
  }
}