package org.clulab.wm.eidos.attachments

import org.clulab.wm.eidos.attachments.{Quantification, Increase, Decrease}
import org.clulab.wm.eidos.test.TestUtils._

class TestEidosAttachment extends Test {

  {
    behavior of "a Quantification"

    val attach1 = new Quantification("trigger", Some(Seq("mod1", "mod2")))
    val attach2 = new Quantification("trigger", Some(Seq("mod2", "mod1")))
    val attach3 = new Quantification("trigger", Some(Seq("mod1", "mod1")))
    val attach4 = new Quantification("reggirt", Some(Seq("mod1", "mod2")))

    it should "equal itself" in {
      attach1 == attach1 should be (true)
    }

    it should "equal something the same" in {
      attach1 == attach2 should be (true)
    }

    it should "not equal something different" in {
      attach1 == attach3 should be (false)
      attach1 == attach4 should be (false)
    }
  }

  {
    behavior of "an Increase"

    val attach1 = new Increase("trigger", Some(Seq("mod1", "mod2")))
    val attach2 = new Increase("trigger", Some(Seq("mod2", "mod1")))
    val attach3 = new Increase("trigger", Some(Seq("mod1", "mod1")))
    val attach4 = new Increase("reggirt", Some(Seq("mod1", "mod2")))

    it should "equal itself" in {
      attach1 == attach1 should be (true)
    }

    it should "equal something the same" in {
      attach1 == attach2 should be (true)
    }

    it should "not equal something different" in {
      attach1 == attach3 should be (false)
      attach1 == attach4 should be (false)
    }
  }

  {
    behavior of "a Decrease"

    val attach1 = new Decrease("trigger", Some(Seq("mod1", "mod2")))
    val attach2 = new Decrease("trigger", Some(Seq("mod2", "mod1")))
    val attach3 = new Decrease("trigger", Some(Seq("mod1", "mod1")))
    val attach4 = new Decrease("reggirt", Some(Seq("mod1", "mod2")))

    it should "equal itself" in {
      attach1 == attach1 should be (true)
    }

    it should "equal something the same" in {
      attach1 == attach2 should be (true)
    }

    it should "not equal something different" in {
      attach1 == attach3 should be (false)
      attach1 == attach4 should be (false)
    }
  }
}
