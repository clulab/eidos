package org.clulab.wm

import org.clulab.odin.Mention


/**
  * These are the functions that we'll be testing, that import from PaperReader
  */


//val eeWithActionsAndGlobal = ExtractorEngine(rules, myActions, myGlobalAction)
object TestUtils {
  
  val agroSystem = new AgroSystem()

  def extractMentions(text: String): Vector[Mention] = agroSystem.extractFrom(text)

}
