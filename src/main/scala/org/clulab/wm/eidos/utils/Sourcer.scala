package org.clulab.wm.eidos.utils

import java.io.File
import scala.io.BufferedSource
import scala.io.Source
import java.nio.charset.StandardCharsets;

object Sourcer {
  val utf8 = StandardCharsets.UTF_8.toString
  
  def sourceFromResource(path: String): BufferedSource = {
    val url = Sourcer.getClass.getResource(path)
    println("Sourcing resource: " + url.getPath())
    val source = Source.fromURL(url, utf8)

    source
  }
  
  def sourceFromFile(path: String): BufferedSource = sourceFromFile(new File(path))
  
  def sourceFromFile(file: File): BufferedSource = {
    val source = Source.fromFile(file, utf8)
    println("Sourcing file: " + file.getCanonicalPath())

    source
  }
}
