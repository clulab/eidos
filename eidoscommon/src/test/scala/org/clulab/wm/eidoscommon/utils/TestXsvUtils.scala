package org.clulab.wm.eidoscommon.utils

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

  it should "escape correctly" in {
    val tsvWriter = new TsvWriter(null)
    val input = "\n\r\t\\"
    val expectedOutput = "\\n\\r\\t\\\\"
    val actualOutput = tsvWriter.escape(input)

    actualOutput should be (expectedOutput)
  }
}