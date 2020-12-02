package org.clulab.wm.eidos.entities

import org.clulab.wm.eidos.extraction.EntityConstraints
import org.clulab.wm.eidos.test.EidosTest

class TestEntityConstraints extends EidosTest {

  def matchBrackets(text: String) =
      EntityConstraints.matchingBrackets(text.split(' ').toSeq)

  behavior of "EntityConstraints"

  it should "approve of properly nested parentheses" in {

    matchBrackets("This has none.") should be (true)
    matchBrackets("This has ( one pair ) .") should be (true)
    matchBrackets("This has ( ( nested pairs ) ) .") should be (true)
    matchBrackets("This has ( [ { } ] ) mixed pairs .") should be (true)
    matchBrackets("This has ( { ) } intermixed pairs .") should be (true)
    matchBrackets("This has -LRB- one strange pair -RRB- .") should be (true)
    matchBrackets("This has ( double ( nesting ) ( of ) parens ) .") should be (true)
  }

  it should "disapprove of improperly nested parentheses" in {
    matchBrackets("This starts with ) a reversed pair ( .") should be (false)
    matchBrackets("This has ( one normal pair ) and ) a reversed pair ( .") should be (false)
    matchBrackets("This count is just uneven ( in this one .") should be (false)
    matchBrackets("and this ) too") should be (false)
  }
}
