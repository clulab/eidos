package org.clulab.wm.eidos.extraction

import org.clulab.odin.Mention
import org.clulab.processors.Document

class EidosReader ( // or do we call it EidosSystem??
                    entityFinders: Seq[Finder],       // 0 or more
                    eventFinders: Seq[Finder],        // 0 or more?
                    //  preProcessor: PreProcessor,       // in case you need to parse stuf
                  ) extends Finder {

  def extract(doc: Document): Seq[Mention] = ???
  //  def extract(text: String): Seq[Mention] = ???

}