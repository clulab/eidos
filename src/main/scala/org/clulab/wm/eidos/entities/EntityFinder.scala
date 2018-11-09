package org.clulab.wm.eidos.entities

import org.clulab.odin.Mention
import org.clulab.processors.Document


trait EntityFinder {

  def extract(doc: Document): Seq[Mention]

  def extractAndFilter(doc: Document): Seq[Mention]

}
