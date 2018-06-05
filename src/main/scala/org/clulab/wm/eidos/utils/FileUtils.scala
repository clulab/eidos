package org.clulab.wm.eidos.utils

import java.io.{File, FileNotFoundException, FilenameFilter, PrintWriter}
import java.util.Collection

import org.clulab.wm.eidos.utils.Sinker.logger
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.io.Source

object FileUtils {

  def printWriterFromFile(file: File): PrintWriter = Sinker.printWriterFromFile(file)

  def printWriterFromFile(path: String): PrintWriter = Sinker.printWriterFromFile(path)

  def findFiles(collectionDir: String, extension: String): Seq[File] = {
    val dir = new File(collectionDir)
    val filter = new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name.endsWith(extension)
    }
    
    val result = dir.listFiles(filter)
    if (result == null)
      throw Sourcer.newFileNotFoundException(collectionDir)
    result
  }

  def getCommentedLinesFromSource(source: Source): Array[String] = {
    try {  
      source
          .getLines()
          .toArray
          // Skips "empty" lines as well as comments
          .filter(line => !line.startsWith("#") && line.trim().nonEmpty)
    }
    finally {
      source.close()
    }
  }
  
  // Add FromFile as necessary.  See getText below.
  def getCommentedTextsFromResource(path: String): Seq[String] =
      getCommentedLinesFromSource(Sourcer.sourceFromResource(path))
          .map(_.trim)
 
  // Add FromResource as necessary.  See getText below,
  def getCommentedTextFromFile(file: File, sep: String = " "): String =
      getCommentedLinesFromSource(Sourcer.sourceFromFile(file))
          // These haven't been trimmed in case esp. trailing spaces are important.
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
      
  def getTextFromFile(path: String): String =
      getTextFromSource(Sourcer.sourceFromFile(path))
  
  def loadYamlFromResource(path: String): Collection[Any] = {
    val input = getTextFromResource(path)
    val yaml = new Yaml(new Constructor(classOf[Collection[Any]]))
    
    yaml.load(input).asInstanceOf[Collection[Any]]
  }
}
