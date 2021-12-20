package org.clulab.wm.eidos.apps.batch

import org.clulab.wm.eidoscommon.utils.FileUtils

import scala.io.Source

object DiffFiles extends App {
  val file1 = args(0)
  val file2 = args(1)

  val files1Set = Source.fromFile(file1).getLines.toSet
  val files2Set = Source.fromFile(file2).getLines.toSet

  val only1 = files1Set.diff(files2Set).toSeq.sorted
  val only2 = files2Set.diff(files1Set).toSeq.sorted

  val both = files1Set.intersect(files2Set)
  println("Only1:")
  only1.foreach(println)

  println()
  println("Only2:")
  only2.foreach(println)

  println()
  println("Both:")
  both.foreach(println)
}
