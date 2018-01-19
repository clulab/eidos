package org.clulab.wm

import org.clulab.odin.Mention

/**
  * These are the functions that we'll be testing, that import from PaperReader
  */

//val eeWithActionsAndGlobal = ExtractorEngine(rules, myActions, myGlobalAction)
object TestUtils {
  val p1s1 = "Food insecurity levels are extremely alarming throughout the country due to conflict, a collapsing economy, low cereal production, poor rainfall in southeastern areas, and the exhaustion of coping capacities after several years of crisis."
  val p1s2 = "In Eastern Equatoria – one of the drought-hit areas – the latest food security and nutrition assessments in November 2016 revealed that households experiencing poor food consumption nearly doubled from 19 to 37 percent when compared with the same time in 2015."
  val p1s3 = "Rainfall deficits were widely reported in the area, with 93 percent of households indicating this was a major shock during the last agricultural season, impacting pasture and water availability and local food production."
  val p1s4 = "Climatic forecasting indicates these areas are likely to experience depressed rainfall between March to May 2017."
  
  val p1 = Array(p1s1, p1s2, p1s3, p1s4).mkString(" ")

  
  
  val agroSystem = new AgroSystem()

  def extractMentions(text: String): Vector[Mention] = agroSystem.extractFrom(text)
}
