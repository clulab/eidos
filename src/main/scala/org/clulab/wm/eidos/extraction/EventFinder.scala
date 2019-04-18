package org.clulab.wm.eidos.extraction

import org.clulab.odin.{ExtractorEngine, Mention}
import org.clulab.processors.Document

class EventFinder(eventEngine: ExtractorEngine) extends Finder {
  def extract(doc: Document): Seq[Mention] = ???
}
