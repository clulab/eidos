package org.clulab.wm

import org.clulab.wm.TestUtils.Test

class TestEidosSystem extends Test {

  it should "compare all entities for sameAs" in {
    val tester = new Tester("The increase in rain causes flooding, conflict,  and a decrease in crop yield.")
    println(tester.mentions.map(_.text).mkString("\t"))

    val entities = tester.mentions.filter(_ matches "Entity")
    entities.length should be (4)

    val sameAsRelations = TestUtils.system.populateSameAsRelations(entities)
    sameAsRelations.length should be(6)

    sameAsRelations.foreach(s => println(utils.DisplayUtils.displayMention(s)))
  }



}
