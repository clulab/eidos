package org.clulab.wm.eidos.refiners

import org.clulab.odin.Mention
import org.clulab.odin.State
import org.clulab.processors.Document
import org.clulab.wm.eidos.components.EidosComponents
import org.clulab.wm.eidos.extraction.Finder

class FinderRefiner(val finder: Finder) extends Refiner("Finder-" + finder.getClass.getSimpleName)

object FinderRefiner {
  protected val emptyState = new State() // Keep this around for reuse.

  def mkRefiners(components: EidosComponents): Seq[FinderRefiner] = {
    components.findersOpt.map { finders =>
      finders.map(new FinderRefiner(_))
    }.getOrElse(Seq.empty)
  }

  def refine(finders: Seq[FinderRefiner], doc: Document, useTimer: Boolean): Seq[Mention] = {
    require(doc.text.isDefined)

    // Perform the information extraction in the sequence set in the config, using Finders
    val extractions = finders.foldLeft(emptyState) { (state, finderRefiner) =>
      finderRefiner.time(useTimer) {
        val mentions = finderRefiner.finder.find(doc, state).toVector

        state.updated(mentions)
      }
    }

    // It is believed that toVector is required to avoid some race conditions within the engine.
    // The engine's extractFrom returns a Seq which may involve delayed evaluation.
    extractions.allMentions
  }
}
