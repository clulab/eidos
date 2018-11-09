package org.clulab.wm.eidos.groundings

import java.io.{FileInputStream, ObjectInputStream}

import org.clulab.utils.ClassLoaderObjectInputStream
import org.clulab.wm.eidos.utils.Namer
import org.slf4j.LoggerFactory

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