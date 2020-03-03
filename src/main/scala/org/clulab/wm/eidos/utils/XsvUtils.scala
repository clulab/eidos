package org.clulab.wm.eidos.utils

import java.io.PrintWriter

class EscapePair(char: Char) {
  val unescaped = char.toString
  val escaped = "\\" + unescaped

  def escape(string: String): String = string.replace(unescaped, escaped)

  def unescape(string: String): String = string.replace(escaped, unescaped)
}

// See https://en.wikipedia.org/wiki/Tab-separated_values.
// This does not attempt to double internal quotes or quote an entire field that contains a quote, etc.
object XsvUtils {
  var nlChar = '\n'
  var crChar = '\r'
  val tabChar = '\t'
  val commaChar = ','
  val quoteChar = '"'
  val backslashChar = '\\'

  val escapePairs = Seq(
    EscapePair(XsvUtils.backslashChar),
    EscapePair(XsvUtils.nlChar),
    EscapePair(XsvUtils.crChar),
    EscapePair(XsvUtils.tabChar)
  )
}

class XsvReader(protected val separatorChar: Char) {
}

class TsvReader() extends XsvReader(XsvUtils.tabChar) {

  def unescape(string: String): String = {
    XsvUtils.escapePairs.reverse.foldLeft(string) { (string, escapePair) => escapePair.unescape(string) }
  }

  def readln(line: String): Array[String] = line
      .split(separatorChar)
      .map(unescape)
}

object TsvReader {
  def unescape(text: String, separatorChar: Char): String = text
      .replace("\\t", "\t")
      .replace("\\r", "\r")
      .replace("\\n", "\n")
      .replace("\\\\", "\\")
}

class CsvReader() extends XsvReader(XsvUtils.commaChar) {
  // TODO It is more complicated because of the multiple lines per string
}

abstract class XsvWriter(printWriter: PrintWriter, separatorChar: Char) {
  protected val separatorString = separatorChar.toString

  def quote(text: String): String = '"' + text.replace("\"", "\"\"") + '"'

  def stringln(strings: String*): String

  def println(strings: String*): Unit = {
    printWriter.println(stringln(strings: _*))
  }

  def close(): Unit = printWriter.close
}

class TsvWriter(printWriter: PrintWriter, isExcel: Boolean = true) extends XsvWriter(printWriter, XsvUtils.tabChar) {

  def escape(string: String): String = {
    XsvUtils.escapePairs.foldLeft(string) { (string, escapePair) => escapePair.escape(string) }
  }

  def stringlnPlain(strings: String*): String = {
    val escapedStrings = strings.map(escape)

    escapedStrings.mkString(separatorString)
  }

  def stringlnExcel(strings: String*): String = {
    val quotedStrings = strings.map { string =>
      val mustBeQuoted = TsvWriter.quoteableStrings.exists { separator: String =>
        string.contains(separator)
      }

      if (mustBeQuoted) quote(string)
      string
    }

    quotedStrings.mkString(separatorString)
  }

  def stringln(strings: String*): String =
      if (isExcel) stringlnExcel(strings: _*)
      else stringlnPlain(strings: _*)
}


object EscapePair {

  def apply(char: Char) = new EscapePair(char)
}

object TsvWriter {
  val quoteableStrings = Seq(
    XsvUtils.nlChar.toString,
    XsvUtils.crChar.toString,
    XsvUtils.tabChar.toString,
    XsvUtils.quoteChar.toString
  )
}

class CsvWriter(printWriter: PrintWriter, isExcel: Boolean = true) extends XsvWriter(printWriter, XsvUtils.commaChar) {
  // TODO: Excel does not seem to be able to handle tabs.

  def stringln(strings: String*): String = {
    val quotedStrings = strings.map { string =>
      val mustBeQuoted = CsvWriter.quotableStrings.exists { separator: String =>
        string.contains(separator)
      }

      if (mustBeQuoted) quote(string)
      string
    }

    quotedStrings.mkString(separatorString)
  }
}

object CsvWriter {
  val quotableStrings = Seq(
    XsvUtils.nlChar.toString,
    XsvUtils.crChar.toString,
    XsvUtils.commaChar.toString,
    XsvUtils.quoteChar.toString
  )
}
