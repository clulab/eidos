package org.clulab.wm.eidos.actions

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin._
import org.clulab.odin.impl.Taxonomy
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.utils.FileUtils
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.collection.mutable.ArrayBuffer

trait CorefHandler {
  def resolveCoref(mentions: Seq[Mention]): Seq[Mention]
}
object CorefHandler {

  // Used for simplistic coreference identification
  val COREF_DETERMINERS: Set[String] = Set("this", "that", "these", "those")

  val ANTECEDENT: String = "antecedent"
  val ANAPHOR: String = "anaphor"

  def fromConfig(config: Config): CorefHandler = {
    config[String]("corefType") match {
      case "causalBasic" => CausalBasicCorefHandler.fromConfig(config)
      case _ => ???
    }
  }

  def hasCorefToResolve(m: Mention): Boolean = {
    m match {
      case tb: TextBoundMention => startsWithCorefDeterminer(tb)
      case rm: RelationMention => existsDeterminerCause(rm)
      case em: EventMention => existsDeterminerCause(em)
      case _ => false
    }
  }

  def startsWithCorefDeterminer(m: Mention): Boolean = {
    val corefDeterminers = COREF_DETERMINERS
    corefDeterminers.exists(det => m.text.toLowerCase.startsWith(det))
  }

  def existsDeterminerCause(mention: Mention): Boolean = {
    if (isCauseEvent(mention)) CorefHandler.startsWithCorefDeterminer(mention.arguments("cause").head)
    else false
  }

  def isCauseEvent(mention: Mention): Boolean = {
    mention.arguments.get("cause").nonEmpty
  }
}

class CausalBasicCorefHandler(taxonomy: Taxonomy) extends CorefHandler {

  def resolveCoref(mentions: Seq[Mention]): Seq[Mention] = {
    val (eventMentions, _) = mentions.partition(_.isInstanceOf[EventMention])

    if (eventMentions.isEmpty) mentions
    else {
      val orderedBySentence = eventMentions.groupBy(_.sentence)
      val numSentences = eventMentions.head.document.sentences.length

      if (orderedBySentence.isEmpty) mentions
      else {
        val resolvedMentions = new ArrayBuffer[Mention]

        for (i <- 1 until numSentences) {
          for (mention <- orderedBySentence.getOrElse(i, Seq.empty[Mention])) {

            // If there is an event with "this/that" as cause...
            if (CorefHandler.existsDeterminerCause(mention)) {

              // Get Causal mentions from the previous sentence (if any)
              val prevSentenceCausal = getPreviousSentenceCausal(orderedBySentence, i)
              if (prevSentenceCausal.nonEmpty) {

                // If there was also a causal event in the previous sentence
                val lastOccurring = prevSentenceCausal.maxBy(_.tokenInterval.end)
                // antecedent
                val prevEffects = lastOccurring.arguments("effect")
                // reference
                val currCauses = mention.asInstanceOf[EventMention].arguments("cause")
                if (prevEffects.nonEmpty && currCauses.nonEmpty) {
                  // todo: cover if there is more than one effect?
                  val antecedent = prevEffects.head
                  val anaphor = currCauses.head
                  // Make a new CrossSentence mention using the previous effect as the anchor
                  // Note: Overly simplistic, this is a first pass
                  // todo: expand approach
                  val corefMention = new CrossSentenceMention(
                    labels = taxonomy.hypernymsFor(EidosSystem.COREF_LABEL),
                    anchor = antecedent,
                    neighbor = anaphor,
                    arguments = Map[String, Seq[Mention]]((CorefHandler.ANTECEDENT, Seq(antecedent)), (CorefHandler.ANAPHOR, Seq(anaphor))),
                    document = mention.document,
                    keep = true,
                    foundBy = s"BasicCorefAction_ant:${lastOccurring.foundBy}_ana:${mention.foundBy}",
                    attachments = Set.empty[Attachment]
                  )
                  resolvedMentions.append(corefMention)

                } else throw new RuntimeException(s"Previous or current Causal mention doesn't have effects " +
                  s"\tsent1: ${lastOccurring.sentenceObj.getSentenceText}\n" +
                  s"\tsent2 ${mention.sentenceObj.getSentenceText}")
              }
            }
          }
        }
        mentions ++ resolvedMentions
      }
    }
  }

  def getPreviousSentenceCausal(orderedBySentence: Map[Int, Seq[Mention]], i: Int): Seq[Mention] = {
    val prevSentenceMentions = orderedBySentence.getOrElse(i - 1, Seq.empty[Mention])
    prevSentenceMentions.filter(_ matches EidosSystem.CAUSAL_LABEL)
  }

}

object CausalBasicCorefHandler {
  def fromConfig(config: Config): CausalBasicCorefHandler = {
    val taxonomyPath = config[String]("taxonomyPath")
    val taxonomy = readTaxonomy(taxonomyPath)
    new CausalBasicCorefHandler(taxonomy)
  }

  def readTaxonomy(path: String): Taxonomy = {
    val input = FileUtils.getTextFromResource(path)
    val yaml = new Yaml(new Constructor(classOf[java.util.Collection[Any]]))
    val data = yaml.load(input).asInstanceOf[java.util.Collection[Any]]
    Taxonomy(data)
  }
}
