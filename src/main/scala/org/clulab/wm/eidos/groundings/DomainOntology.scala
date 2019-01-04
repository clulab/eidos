package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.utils.Namer

import scala.util.matching.Regex

trait DomainOntology {

  def size: Integer

  def getNamer(n: Integer): Namer

  def getValues(n: Integer): Array[String]

  def getPatterns(n: Integer): Option[Array[Regex]]

  def save(filename: String): Unit
}

object DomainOntology {
  val ESCAPE = "\\"
  val ESCAPED_ESCAPE = ESCAPE + ESCAPE
  val SEPARATOR = "/"
  val ESCAPED_SEPARATOR = ESCAPE + SEPARATOR
}