package org.clulab.wm.eidos.apps.batch

import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.StringUtils

import scala.io.Source

object DiffList extends App {
  val file = args(0)
  val dir = args(1)

  val fileSet = Source.fromFile(file).getLines.toSet
  val dirSet = FileUtils.findFiles(dir, ".jsonld")
      .map(_.getName)
      .map(StringUtils.beforeLast(_, '.'))
      .toSet
  val onlyFile = fileSet.diff(dirSet).toSeq.sorted
  val onlyDir = dirSet.diff(fileSet).toSeq.sorted
  val both = fileSet.intersect(dirSet)

  println("onlyFile:")
  onlyFile.foreach(println)
  println()
  println("onlyDir:")
  onlyDir.foreach(println)
  println()
  println("both:")
  both.foreach(println)
}
