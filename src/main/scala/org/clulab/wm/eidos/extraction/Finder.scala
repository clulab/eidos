package org.clulab.wm.eidos.extraction

import org.clulab.odin.Mention
import org.clulab.processors.Document

trait Finder {
  def extract(doc: Document): Seq[Mention]
}