package ai.lum.eidos.sparql.utils

import java.io.PrintWriter

object TsvUtils {
  val separatorChar = '\t'
  val separatorString = separatorChar.toString

  class TsvReader() {

    def unescape(text: String): String = text
        .replace("\\t", "\t")
        .replace("\\r", "\r")
        .replace("\\n", "\n")
        .replace("\\\\", "\\")

    def split(text: String): Array[String] = text.split(' ')

    def readln(line: String): Array[String] = line.split(separatorChar).map(unescape)
  }

  class TsvWriter(printWriter: PrintWriter) {

    def escape(text: String): String = text
        .replace("\\", "\\\\")
        .replace("\n", "\\n")
        .replace("\r", "\\r")
        .replace("\t", "\\t")

    def println(strings: String*): Unit = printWriter.println(strings.map(escape).mkString(separatorString))

    def close(): Unit = printWriter.close()

    def flush(): Unit = printWriter.flush()
  }
}
