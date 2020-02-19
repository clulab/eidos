package ai.lum.eidos.sparql.utils

import java.io.BufferedOutputStream
import java.io.FileOutputStream
import java.io.{File, OutputStreamWriter, PrintWriter}
import java.nio.charset.StandardCharsets

class Sink(file: File, charsetName: String, append: Boolean = false) extends OutputStreamWriter(
  if (append) FileUtils.newAppendingBufferedOutputStream(file) else FileUtils.newBufferedOutputStream(file),
  charsetName)

object Sinker {
  val utf8: String = StandardCharsets.UTF_8.toString

  def printWriterFromFile(file: File, append: Boolean): PrintWriter = {
    new PrintWriter(new Sink(file, Sourcer.utf8, append))
  }

  def printWriterFromFile(path: String, append: Boolean = false): PrintWriter = printWriterFromFile(new File(path), append)
}
