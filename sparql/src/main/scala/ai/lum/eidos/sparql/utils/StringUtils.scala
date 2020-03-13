package ai.lum.eidos.sparql.utils

object StringUtils {

  def before(string: String, index: Int, all: Boolean): String = {
    if (index < 0)
      if (all) string
      else ""
    else string.substring(0, index)
  }

  def beforeLast(string: String, char: Char, all: Boolean = true): String =
      before(string, string.lastIndexOf(char), all)

  def beforeFirst(string: String, char: Char, all: Boolean = true): String =
      before(string, string.indexOf(char), all)

  def after(string: String, index: Int, all: Boolean): String = {
    if (index < 0)
      if (all) string
      else ""
    else string.substring(index + 1)
  }

  def afterLast(string: String, char: Char, all: Boolean = true): String =
      after(string, string.lastIndexOf(char), all)

  def afterFirst(string: String, char: Char, all: Boolean = true): String =
      after(string, string.indexOf(char), all)
}
