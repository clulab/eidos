package org.clulab.wm.eidos.extraction

import org.clulab.odin.Mention
import org.clulab.processors.Document
import org.clulab.wm.eidos.Expander

class GazetteerEntityFinder(lexicons: Seq[String], expander: Option[Expander]) extends Finder {
  def extract(doc: Document): Seq[Mention] = ???
}
