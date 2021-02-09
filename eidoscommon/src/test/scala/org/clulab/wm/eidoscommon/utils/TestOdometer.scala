package org.clulab.wm.eidoscommon.utils

class TestOdometer extends Test {
  
  behavior of "SeqOdometer"
  
  it should "count" in {
    val seq1 = Seq[Integer](1, 2, 3)
    val seq2 = Seq[Integer](10, 20, 30)
    val odometer = new SeqOdometer[Integer](Array(seq1, seq2))

    var length = 0
    odometer.foreach { values: Seq[Integer] =>
      length += 1
      values.foreach(print)
      println
    }
    length should be (seq1.length * seq2.length)
  }

  behavior of "RangeOdometer"

  it should "count" in {
    val range1 = 1.to(3)
    val range2 = 10.to(30).by(10)
    val odometer = new RangeOdometer(Array(range1, range2))

    var length = 0
    odometer.foreach { values: Seq[Int] =>
      length += 1
      values.foreach(print)
      println
    }
    length should be (range1.length * range2.length)
  }
}
