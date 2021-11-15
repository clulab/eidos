package org.clulab.wm.eidoscommon.utils

object TakeTest extends App {
  val seq = Seq(1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1)
  val taken = seq.take(11)
  val equals = seq.eq(taken)
  private val stuff: Boolean = true
  println(equals)

  println(-Int.MinValue)
}
