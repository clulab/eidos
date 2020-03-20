package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.test.TestUtils.Test

class TestSanitizedWord2Vec extends Test {
  val path = this.defaultConfig.getString("ontologies.wordToVecPath")
  val sanitizedWord2Vec = new SanitizedWord2VecBuilder().build(path, resource = true, cached = false)

  behavior of "SanitizedWord2Vec"

  it should "have an unknown vector" in {
    val unknownVector = sanitizedWord2Vec.unknownVector

    unknownVector.exists(_ == 0f) should be (false)
  }

  it should "have a consistent unknown vector" in {
    val unknownVector = sanitizedWord2Vec.unknownVector
    val emptyVectorOpt = sanitizedWord2Vec.makeCompositeVector(Seq(""))
    // The first will match, the second won't be found and back off to unknown.
    val pairedVectorOpt = sanitizedWord2Vec.makeCompositeVector(Seq("", "asdf"))

    emptyVectorOpt shouldBe defined
    emptyVectorOpt.get should === (unknownVector)

    pairedVectorOpt shouldBe defined
    pairedVectorOpt.get should === (unknownVector)
  }

  it should "not produce None if completely irrelevant" in {
    val oov1Opt = sanitizedWord2Vec.makeCompositeVector(Seq.empty)
    val oov2Opt = sanitizedWord2Vec.makeCompositeVector(Seq("asdf"))
    val oov3Opt = sanitizedWord2Vec.makeCompositeVector(Seq("asdf", "qwerty"))

    oov1Opt shouldBe empty
    oov2Opt shouldBe empty
    oov3Opt shouldBe empty
  }

  // Test some backing off.
}
