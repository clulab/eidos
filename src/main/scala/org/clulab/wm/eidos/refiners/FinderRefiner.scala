package org.clulab.wm.eidos.refiners

import org.clulab.odin.Mention
import org.clulab.odin.State
import org.clulab.processors.Document
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.components.EidosComponents
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.utils.Timer

object FinderRefiner {
  protected val emptyState = new State() // Keep this around for reuse.

  def mkFinderRefiners(components: EidosComponents): Seq[Finder] = {
    components.findersOpt.getOrElse(Seq.empty)
  }

  def mkMentions(finders: Seq[Finder], doc: Document, useTimer: Boolean): Seq[Mention] = {
    require(doc.text.isDefined)

    // Perform the information extraction in the sequence set in the config, using Finders
    val extractions = finders.foldLeft(emptyState) { (state, finder) =>
      Timer.time("Run " + finder.getClass.getSimpleName, useTimer) {
        val mentions = finder.find(doc, state).toVector

        state.updated(mentions)
      }
    }

    // It is believed that toVector is required to avoid some race conditions within the engine.
    // The engine's extractFrom returns a Seq which may involve delayed evaluation.
    extractions.allMentions
  }
}
