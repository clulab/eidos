package org.clulab.wm.eidos.utils

import java.io.{File, FileOutputStream, OutputStreamWriter, PrintWriter}
import java.nio.charset.StandardCharsets

import org.slf4j.LoggerFactory

class Sink(file: File, csn: String) extends OutputStreamWriter(new FileOutputStream(file), csn)

object Sinker {
  val logger = LoggerFactory.getLogger(this.getClass())
  val utf8 = StandardCharsets.UTF_8.toString

  def printWriterFromFile(file: File): PrintWriter = {
    logger.info("Sinking file " + file.getPath())

    new PrintWriter(new Sink(file, Sourcer.utf8))
  }

  def printWriterFromFile(path: String): PrintWriter = printWriterFromFile(new File(path))
}
