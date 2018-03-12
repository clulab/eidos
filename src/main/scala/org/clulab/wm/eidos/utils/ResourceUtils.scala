package org.clulab.wm.eidos.utils

import java.io.InputStream


object ResourceUtils {

  // methods for reading rules

  def streamFromResource(path: String): InputStream = {
    val stream = getClass.getClassLoader.getResourceAsStream(path)
    stream
  }

  def readResource(path: String): String = {
    val stream = streamFromResource(path)
    val source = scala.io.Source.fromInputStream(stream)
    val data = source.mkString
    source.close()
    data
  }

  def readStrings(path: String, delimiter: String = ","): Seq[String] = {
    readResource(path).split(delimiter).map(_.trim)
  }
}
