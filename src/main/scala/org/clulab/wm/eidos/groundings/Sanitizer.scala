package org.clulab.wm.eidos.groundings

class Sanitizer {

  protected def isNumber(word: String): Boolean =
  // There is no char that doesn't look numbery and there is some digit.
    !word.exists(!"0123456789-+,./\\".contains(_)) && word.exists("0123456789".contains(_))

  protected def isUrl(word: String): Boolean = {
    val lowerWord = word.toLowerCase

    // Skip URLS.  Add .com and .org to cover more urls (becky).
    lowerWord.startsWith("http") || lowerWord.contains(".com") || lowerWord.contains(".org")
  }

  def sanitize(word: String): Option[String] = {
    val sanitizedWord = word
        .filter { char =>
          Character.isLetter(char) || "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~".contains(char)
        }
        .map { char =>
          if (Character.isLetter(char)) char.toLower
          else '_'
        }

    if (sanitizedWord.isEmpty) None
    else Some(sanitizedWord)
  }
}
