package org.clulab.wm

import org.scalatest._
import TestUtils._
import ReaderUtils._

class TestCausal extends FlatSpec with Matchers {

  //val sent3 = "Limited financial capacities and low education levels further restrict farmers’ ability for higher benefits from increased agricultural production."
  // (DEC-limited) financial capacities --> (DEC) farmer's ability for higher benefits
  // (QUANT-low) education levels
  // (INC-inc) agricultural production
  //val mentions3 = extractMentions(sent3)

}
