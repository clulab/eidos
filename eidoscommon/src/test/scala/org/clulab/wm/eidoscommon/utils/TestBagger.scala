package org.clulab.wm.eidoscommon.utils

class TestBagger extends Test {
  
  behavior of "Equality Bagger"

  case class Bagged(value: Int)

  it should "keep accurate count when added one at a time" in {
    val bagger = new EqualityBagger[Bagged]()

    bagger
        .put(Bagged(0))
        .put(Bagged(0))
        .put(Bagged(1))

    bagger.get(Bagged(0)) should be (2)
    bagger.get(Bagged(1)) should be (1)
    bagger.get(Bagged(2)) should be (0)
  }

  it should "keep accurate count when added multiple at a time" in {
    val bagger = new EqualityBagger[Bagged]()

    bagger
        .put(Seq(Bagged(0), Bagged(0)))
        .put(Seq(Bagged(1)))

    bagger.get(Bagged(0)) should be (2)
    bagger.get(Bagged(1)) should be (1)
    bagger.get(Bagged(2)) should be (0)
  }

  behavior of "Identity Bagger"

  val bagged0 = Bagged(0)
  val bagged1 = Bagged(1)
  val bagged2 = Bagged(2)

  it should "keep accurate count when added one at a time" in {
    val bagger = new IdentityBagger[Bagged]()

    bagger
        .put(bagged0)
        .put(bagged0)
        .put(bagged1)

    bagger.get(bagged0) should be (2)
    bagger.get(bagged1) should be (1)
    bagger.get(bagged2) should be (0)
    bagger.get(Bagged(0)) should be (0)
  }

  it should "keep accurate count when added multiple at a time" in {
    val bagger = new IdentityBagger[Bagged]()

    bagger
        .put(Seq(bagged0, bagged0))
        .put(Seq(bagged1))

    bagger.get(bagged0) should be (2)
    bagger.get(bagged1) should be (1)
    bagger.get(bagged2) should be (0)
    bagger.get(bagged2) should be (0)
    bagger.get(Bagged(0)) should be (0)
  }
}
