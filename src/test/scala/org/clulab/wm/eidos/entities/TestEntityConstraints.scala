package org.clulab.wm.eidos.entities

import org.clulab.wm.eidos.test.TestUtils._

import scala.collection.immutable.HashSet

class TestEntityConstraints extends Test {

  def matchBrackets(text: String) =
      EntityConstraints.matchingBrackets(text.split(' ').toSeq)

  behavior of "EntityConstraints"

  it should "approve of properly nested parentheses" in {

    matchBrackets("This has none.") should be (true)
    matchBrackets("This has ( one pair ) .") should be (true)
    matchBrackets("This has ( ( nested pairs ) ) .") should be (true)
  }

  it should "disapprove on improperly nested parentheses" in {
    matchBrackets("This starts with ) a reversed pair ( .") should be(false)
    matchBrackets("This has ( one normal pair ) and ) a reversed pair ( .") should be(false)
    matchBrackets("This count is just uneven ( in this one.") should be(false)
    matchBrackets("and this ) too") should be(false)
  }
}
