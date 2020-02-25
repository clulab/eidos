package org.clulab.wm.eidos.apps.batch.utils

import java.io.PrintWriter

// This is borrowed from the sparql project and was pasted here so as not to impact Eidos yet.
object TsvUtils {
  val separatorChar = '\t'
  val separatorString: String = separatorChar.toString

  def escape(text: String): String = text
      .replace("\\", "\\\\")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")

  def quoted(text: String): String = '"' + text.replace("\"", "\"\"") + '"'

  def escapeExcel(text: String): String = {
    val escaped = escape(text)

    if (escaped.indexOf('"') >= 0) quoted(escaped)
    else escaped
  }

  class TsvWriter(printWriter: PrintWriter, isExcel: Boolean = true) {

    def println(strings: String*): Unit = {
      val escapedStrings =
        if (isExcel) strings.map(escapeExcel)
        else strings.map(escape)

      printWriter.println(escapedStrings.mkString(separatorString))
    }

    def close(): Unit = printWriter.close()
  }
}
