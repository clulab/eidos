package org.clulab.wm

import org.scalatest._

import org.clulab.odin.Mention

class AgroTest extends FlatSpec with Matchers

/**
  * These are the functions that we'll be testing, that import from PaperReader
  */

//val eeWithActionsAndGlobal = ExtractorEngine(rules, myActions, myGlobalAction)
object TestUtils {
  val agroSystem = new AgroSystem()

  def extractMentions(text: String): Vector[Mention] = agroSystem.extractFrom(text)
}
