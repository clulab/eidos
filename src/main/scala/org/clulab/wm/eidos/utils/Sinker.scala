package org.clulab.wm.eidos.utils

import java.io.{File, OutputStreamWriter, PrintWriter}
import java.nio.charset.StandardCharsets

import org.slf4j.{Logger, LoggerFactory}

class Sink(file: File, charsetName: String, append: Boolean = false) extends OutputStreamWriter(
  if (append) FileUtils.newAppendingBufferedOutputStream(file) else FileUtils.newBufferedOutputStream(file),
  charsetName)

object Sinker {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)
  val utf8: String = StandardCharsets.UTF_8.toString

  def printWriterFromFile(file: File, append: Boolean): PrintWriter = {
    logger.info("Sinking file " + file.getPath)

    new PrintWriter(new Sink(file, Sourcer.utf8, append))
  }

  def printWriterFromFile(path: String, append: Boolean = false): PrintWriter = printWriterFromFile(new File(path), append)
}
