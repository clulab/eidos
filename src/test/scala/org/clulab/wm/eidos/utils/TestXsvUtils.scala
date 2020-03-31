package org.clulab.wm.eidos.utils

import org.clulab.wm.eidos.test.TestUtils._

class TestXsvUtils extends Test {
  
  behavior of "XsvUtils"
  
  it should "split correctly" in {
    val tabs = "one\ttwo\three\t\t"
    val tsvReader = new TsvReader()

    tabs.split('\t') should have length 3
    tsvReader.readln(tabs) should have length 5
  }

  it should "truncate and expand correctly" in {
    val tabs = "one\ttwo\three\t\t"
    val tsvReader = new TsvReader()

    tsvReader.readln(tabs, 4) should have length 4
    tsvReader.readln(tabs, 5) should have length 5
    tsvReader.readln(tabs, 6) should have length 6
  }
}