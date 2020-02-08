package ai.lum.eidos.sparql.utils

import java.io.BufferedOutputStream
import java.io.FileOutputStream
import java.io.{File, OutputStreamWriter, PrintWriter}
import java.nio.charset.StandardCharsets

import org.slf4j.{Logger, LoggerFactory}

class Sink(file: File, charsetName: String) extends OutputStreamWriter(Sinker.newBufferedOutputStream(file), charsetName)

object Sinker {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)
  val utf8: String = StandardCharsets.UTF_8.toString

  def printWriterFromFile(file: File): PrintWriter = {
    logger.info("Sinking file " + file.getPath)

    new PrintWriter(new Sink(file, Sourcer.utf8))
  }

  def printWriterFromFile(path: String): PrintWriter = printWriterFromFile(new File(path))

  def newBufferedOutputStream(file: File): BufferedOutputStream =
    new BufferedOutputStream(new FileOutputStream(file))
}
