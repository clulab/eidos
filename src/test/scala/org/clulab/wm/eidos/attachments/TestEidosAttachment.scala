package org.clulab.wm.eidos.attachments

import org.clulab.wm.eidos.test.TestUtils._

import scala.collection.immutable.HashSet

class TestEidosAttachment extends EidosTest {

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

  {
    behavior of "a set of attachments"

    val mod12 = Some(Seq("mod1", "mod2"))
    val mod21 = Some(Seq("mod2", "mod1"))
    val mod11 = Some(Seq("mod1", "mod1"))

    val attach1a = new Quantification("trigger", mod12)
    val attach1b = new Increase("trigger", mod12)
    val attach1c = new Decrease("trigger", mod12)

    val attach2a = new Quantification("trigger", mod21)
    val attach2b = new Increase("trigger", mod21)
    val attach2c = new Decrease("trigger", mod21)

    val attach3a = new Quantification("trigger", mod11)
    val attach3b = new Increase("trigger", mod11)
    val attach3c = new Decrease("trigger", mod11)

    it should "equal itself" in {
      HashSet(attach1a) == HashSet(attach1a) should be (true)
      HashSet(attach1b) == HashSet(attach1b) should be (true)
      HashSet(attach1c) == HashSet(attach1c) should be (true)
    }

    it should "equal its equivalent" in {
      HashSet(attach1a) == HashSet(attach2a) should be (true)
      HashSet(attach1b) == HashSet(attach2b) should be (true)
      HashSet(attach1c) == HashSet(attach2c) should be (true)
    }

    it should "not be equal if not equivalent" in {
      HashSet(attach1a) == HashSet(attach3a) should be (false)
      HashSet(attach1b) == HashSet(attach3b) should be (false)
      HashSet(attach1c) == HashSet(attach3c) should be (false)
    }

    it should "have equal hashCodes for equal objects" in {
      attach1a == attach2a should be (attach1a.## == attach2a.##)
      attach1b == attach2b should be (attach1b.## == attach2b.##)
      attach1c == attach2c should be (attach1c.## == attach2c.##)

      attach1a == attach3a should be (attach1a.## == attach3a.##)
      attach1b == attach3b should be (attach1b.## == attach3b.##)
      attach1c == attach3c should be (attach1c.## == attach3c.##)
    }
  }
}
