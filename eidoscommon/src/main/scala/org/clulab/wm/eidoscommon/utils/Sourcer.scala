package org.clulab.wm.eidoscommon.utils

import java.io.{File, FileNotFoundException}
import java.nio.charset.StandardCharsets

import scala.io.{BufferedSource, Source}

object Sourcer extends Logging {
  val utf8: String = StandardCharsets.UTF_8.toString

  def sourceFromResource(path: String): BufferedSource = {
    val url = Option(Sourcer.getClass.getResource(path))
      .getOrElse(throw newFileNotFoundException(path))

    logger.info("Sourcing resource " + url.getPath)
    Source.fromURL(url, utf8)
  }

  def sourceFromFile(file: File): BufferedSource = {
    logger.info("Sourcing file " + file.getPath)
    Source.fromFile(file, utf8)
  }

  def sourceFromFile(path: String): BufferedSource = sourceFromFile(new File(path))

  def newFileNotFoundException(path: String): FileNotFoundException = {
    val message1 = path + " (The system cannot find the path specified"
    val message2 = message1 + (if (path.startsWith("~")) ".  Make sure to not use the tilde (~) character in paths in lieu of the home directory." else "")
    val message3 = message2 + ")"

    new FileNotFoundException(message3)
  }
}
