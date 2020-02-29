package ai.lum.eidos.sparql.utils

import java.io.PrintWriter

// See https://en.wikipedia.org/wiki/Tab-separated_values.
// This does not attempt to double internal quotes or quote an entire field that contains a quote, etc.
object TsvUtils {
  val separatorChar = '\t'
  val separatorString: String = separatorChar.toString

  def unescape(text: String): String = text
      .replace("\\t", "\t")
      .replace("\\r", "\r")
      .replace("\\n", "\n")
      .replace("\\\\", "\\")

  def escape(text: String): String = text
      .replace("\\", "\\\\")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")

  def quoted(text: String): String = '"' + text.replace("\"", "\"\"") + '"'

  def escapeExcel(text: String): String = {
    val escaped = escape(text)

    if (escaped.indexOf('"') >= 0)
      quoted(escaped)
    else
      escaped
  }

  def stringln(strings: String*): String = strings.map(escape).mkString(separatorString)

  def readln(line: String): Array[String] = line.split(separatorChar.toString, -1).map(unescape)

  object TsvReader {

  }

  class TsvWriter(printWriter: PrintWriter, isExcel: Boolean = true) {

    def println(strings: String*): Unit = {
      val escapedStrings =
          if (isExcel) strings.map(escapeExcel)
          else strings.map(escape)

      printWriter.println(escapedStrings.mkString(separatorString))
    }

    def close(): Unit = printWriter.close()

    def flush(): Unit = printWriter.flush()
  }
}