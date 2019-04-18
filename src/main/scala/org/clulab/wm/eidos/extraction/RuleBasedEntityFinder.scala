package org.clulab.wm.eidos.extraction

import org.clulab.odin.{ExtractorEngine, Mention}
import org.clulab.processors.Document
import org.clulab.wm.eidos.Expander


class RuleBasedEntityFinder(expander: Option[Expander], val entityEngine: ExtractorEngine, val avoidEngine: ExtractorEngine) extends Finder {
  def extract(doc: Document): Seq[Mention] = ???
}