package org.clulab.wm.eidos.utils

import java.io.File
import java.nio.charset.StandardCharsets;

import org.slf4j.LoggerFactory

import scala.io.BufferedSource
import scala.io.Source

object Sourcer {
  val logger = LoggerFactory.getLogger(this.getClass())
  val utf8 = StandardCharsets.UTF_8.toString
  
  def sourceFromResource(path: String): BufferedSource = {
    val url = Sourcer.getClass.getResource(path)
    logger.info("Sourcing resource " + url.getPath())
    val source = Source.fromURL(url, utf8)

    source
  }
  
  def sourceFromFile(file: File): BufferedSource = {
    val source = Source.fromFile(file, utf8)
    logger.info("Sourcing file " + file.getPath())

    source
  }

  def sourceFromFile(path: String): BufferedSource = sourceFromFile(new File(path))
}
