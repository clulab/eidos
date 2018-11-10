package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.utils.Namer

trait DomainOntology {

  def size: Integer

  def getNamer(n: Integer): Namer

  def getValues(n: Integer): Array[String]

  def save(filename: String): Unit
}

object DomainOntology {
  val ESCAPE = "\\"
  val ESCAPED_ESCAPE = ESCAPE + ESCAPE
  val SEPARATOR = "/"
  val ESCAPED_SEPARATOR = ESCAPE + SEPARATOR
}