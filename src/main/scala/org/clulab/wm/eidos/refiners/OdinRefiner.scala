package org.clulab.wm.eidos.refiners

import org.clulab.odin.Mention
import org.clulab.wm.eidos.components.EidosComponents
import org.clulab.wm.eidos.utils.Timer

class OdinRefiner(name: String, val refine: Seq[Mention] => Option[Seq[Mention]]) extends Refiner(name)

object OdinRefiner {

  // This abbreviated collection is used in a couple of apps that do not need the entire pipeline.
  // Note: In main pipeline we filter to only CAG relevant after this method.  Since the filtering happens at the
  // next stage, currently all mentions make it to the webapp, even ones that we filter out for the CAG exports.
  // val cagRelevant = keepCAGRelevant(events)
  def mkHeadOdinRefiners(components: EidosComponents, options: RefinerOptions): Seq[OdinRefiner] = Seq(
    // Merge attachments: look for mentions with the same span and label and merge their attachments so none get lost
    new OdinRefiner("AttachmentHandler",        (odinMentions: Seq[Mention]) => {
      components.attachmentHandlerOpt.map(_.mergeAttachments(odinMentions))
    }),
    // Keep the most complete version of redundant mentions, should be done *after* attachments have been merged
    new OdinRefiner("MostCompleteEventsKeeper", (odinMentions: Seq[Mention]) => {
      components.mostCompleteEventsKeeperOpt.map(_.keepMostCompleteEvents(odinMentions))
    }),
    // Make sure there are no duplicated Mentions
    new OdinRefiner("Distinct",                 (odinMentions: Seq[Mention]) => {
      Some(odinMentions.distinct)
    })
  )

  // This is the pipeline for odin Mentions.
  def mkTailOdinRefiners(components: EidosComponents, options: RefinerOptions): Seq[OdinRefiner] = Seq(
    // Try to find additional causal relations by resolving simple event coreference
    new OdinRefiner("CorefHandler",           (odinMentions: Seq[Mention]) => {
      components.corefHandlerOpt.map(_.resolveCoref(odinMentions))
    }),
    // Expand concepts that aren't part of causal events, but which we are keeping and outputting
    new OdinRefiner("ConceptExpander",        (odinMentions: Seq[Mention]) => {
      components.conceptExpanderOpt.map(_.expand(odinMentions))
    }),
    // Expand any nested arguments that got missed before
    new OdinRefiner("NestedArgumentExpander", (odinMentions: Seq[Mention]) => {
      components.nestedArgumentExpanderOpt.map(_.traverse(odinMentions))
    }),
    // Filtering based on contentfulness
    new OdinRefiner("StopwordManager",        (odinMentions: Seq[Mention]) => {
      // This exception is dependent on runtime options.
      if (options.cagRelevantOnly) components.stopwordManagerOpt.map(_.keepCAGRelevant(odinMentions))
      else Some(odinMentions)
    }),
    // Annotate hedging
    new OdinRefiner("HedgingHandler",         (odinMentions: Seq[Mention]) => {
      components.hedgingHandlerOpt.map(_.detectHypotheses(odinMentions))
    }),
    // Annotate negation
    new OdinRefiner("NegationHandler",        (odinMentions: Seq[Mention]) => {
      components.negationHandlerOpt.map(_.detectNegations(odinMentions))
    })
  )

  // This is the pipeline for odin Mentions.
  def mkRefiners(components: EidosComponents, options: RefinerOptions): Seq[OdinRefiner] = {
    mkHeadOdinRefiners(components, options) ++ mkTailOdinRefiners(components, options)
  }

  def refine(odinRefiners: Seq[OdinRefiner], odinMentions: Seq[Mention], useTimer: Boolean): Seq[Mention] = {
    val lastMentions = odinRefiners.foldLeft(odinMentions) { (prevMentions, refiner) =>
      refiner.time(useTimer) {
        val nextMentions = refiner
            .refine(prevMentions)
            .getOrElse(prevMentions)

        nextMentions // inspect here
      }
    }
    lastMentions
  }
}
