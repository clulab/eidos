package org.clulab.wm.eidos.utils

import java.io.{File, FileNotFoundException}
import java.net.URL
import java.nio.charset.StandardCharsets

import org.slf4j.LoggerFactory

import scala.io.BufferedSource
import scala.io.Source

object Sourcer {
  val logger = LoggerFactory.getLogger(this.getClass())
  val utf8 = StandardCharsets.UTF_8.toString

  // Keith wrote this, i moved it to it's own method to use it... was part of the below which works
  def resourceURL(path: String): URL = {
    val url = Sourcer.getClass.getResource(path)

    if (url == null)
      throw newFileNotFoundException(path)

    url
  }

  def sourceFromResource(path: String): BufferedSource = {
    val url = resourceURL(path)
    logger.info("Sourcing resource " + url.getPath())
    Source.fromURL(url, utf8)
  }
  
  def sourceFromFile(file: File): BufferedSource = {
    logger.info("Sourcing file " + file.getPath())
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
