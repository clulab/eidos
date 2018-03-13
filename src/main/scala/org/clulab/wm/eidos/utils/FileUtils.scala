package org.clulab.wm.eidos.utils

import java.io.{File, FilenameFilter}
import java.util.Collection

import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.collection.mutable.ListBuffer
import scala.io.Source

object FileUtils {

  import org.clulab.wm.eidos.Aliases._

  def findFiles(collectionDir: String, extension: String): Seq[File] = {
    val dir = new File(collectionDir)
    val filter = new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name.endsWith(extension)
    }
    
    dir.listFiles(filter)
  }

  def getCommentedLinesFromSource(source: Source): Array[String] = {
    try {  
      source
          .getLines()
          .toArray
          .filter(line => !line.startsWith("#"))
          .filter(line => line.nonEmpty)
    }
    finally {
      source.close()
    }
  }
  
  def getCommentedTextsFromResource(path: String): Seq[String] =
      getCommentedLinesFromSource(Sourcer.sourceFromResource(path))
          .map(_.trim)
 
  def getCommentedTextFromFile(file: File, sep: String = " "): String =
      getCommentedLinesFromSource(Sourcer.sourceFromFile(file))
          .mkString(sep)
  
  protected def getTextFromSource(source: Source): String = {
    try {
      source.mkString
    }
    finally {
      source.close()
    }
  }
  
  def getTextFromResource(path: String): String =
      getTextFromSource(Sourcer.sourceFromResource(path))
  
  def getTextFromFile(file: File): String =
      getTextFromSource(Sourcer.sourceFromFile(file))
  
  def loadYamlFromResource(path: String): Collection[Any] = {
    val input = getTextFromResource(path)
    val yaml = new Yaml(new Constructor(classOf[Collection[Any]]))
    
    yaml.load(input).asInstanceOf[Collection[Any]]
  }
}
