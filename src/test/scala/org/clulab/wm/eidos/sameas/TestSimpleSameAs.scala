package org.clulab.wm.eidos.sameas

import org.clulab.wm.eidos.test.TestUtils
import org.clulab.wm.eidos.test.TestUtils.Test
import org.clulab.wm.eidos.utils

class TestSimpleSameAs extends Test {

  it should "compare all entities for sameAs" in {
    val tester = new Tester("The increase in rain causes flooding and conflict.  Also, crop yield decreased.")
//    println(tester.mentions.map(_.text).mkString("\t"))

    val entities = tester.mentions.filter(_ matches "Entity")
    if (TestUtils.ieSystem.word2vec)
      entities.length should be (4)
    else
      entities.length should be (4)
      

    val sameAsRelations = TestUtils.ieSystem.populateSameAsRelations(entities)
    if (TestUtils.ieSystem.word2vec)
      sameAsRelations.length should be (6)
    else
      sameAsRelations.length should be (6)

    // todo: why does this crash when comparing entities across sentences?
    // works fine with: "The increase in rain causes flooding, conflict, and decreased crop yield."
//    sameAsRelations.foreach(s => println(utils.DisplayUtils.displayMention(s)))
  }



}
