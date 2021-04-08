package org.clulab.sbt

import sbt.IO
import sbt.MavenRepository

import java.io.File
import java.util.Properties

object BuildUtils {

  def singleLine(text: String): String = text.stripMargin.replace('\n', ' ').trim

  // See https://stackoverflow.com/questions/25665848/how-to-load-setting-values-from-a-java-properties-file.
  def getProperty(fileName: String, propertyName: String): String = {
    val properties = {
      val properties = new Properties()
      IO.load(properties, new File(fileName))
      properties
    }
    val property = properties.getProperty(propertyName)
    // println(s"$fileName:$propertyName = $property")
    property
  }

  def keepRepos(prefix: String) = (repo: MavenRepository) => {
    repo.root.startsWith(prefix)
  }

  // Avoid in particular those starting with "file:"
  val keepHttpRepos = keepRepos("http")

  def isWindows(): Boolean = {
    System.getProperty("os.name").toLowerCase().contains("win")
  }
  
  val useArtifactory = true
}

