package org.clulab.wm.eidos.utils

import java.util.{Map => JMap}

import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

object OdinUtils {
  // According to https://arxiv.org/pdf/1509.07513.pdf,
  // An exact string matcher is denoted using a string literal, which is a single- or double-quote delimited
  // string. The escape character is the backslash (e.g., \). If the string is a valid Java identifier, the quotes
  // can be omitted. For example, word=dog matches the word “dog”.
  // So, to be on the safe side, escape escape, escape existing quotes, add surrounding quotes.
  // Furthermore, text needs to stay on a single line, so also translate \n and \r.
  def escapeExactStringMatcher(string: String): String = {
    val escaped = string
        .replaceAllLiterally("\\", "\\\\")
        .replaceAllLiterally("\"", "\\\"")
        .replaceAllLiterally("\n", "\\n")
        .replaceAllLiterally("\r", "\\r")

    '"' + escaped + '"'
  }
}
