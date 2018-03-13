package org.clulab.wm.eidos.utils

import java.io.{File, FilenameFilter}
import java.util.Collection
import java.util.jar.JarFile

import scala.collection.mutable.ListBuffer
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.EidosSystem.{INTERCEPT, MU_COEFF, SIGMA_COEFF}
import org.clulab.wm.eidos.EidosSystem

import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

object FileUtils {

  import org.clulab.wm.eidos.Aliases._

  def findFiles(collectionDir: String, extension: String): Seq[File] = {
    val dir = new File(collectionDir)
    val filter = new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name.endsWith(extension)
    }
    
    dir.listFiles(filter)
  }

  def readRules(rulesPath: String): String = {
    println(s"rulesPath:\t$rulesPath")
    val source = Sourcer.sourceFromURL(rulesPath)
    val rules = source.mkString
    source.close()
    rules
  }
  
  def readResource(path: String): String = {
    val source = Sourcer.sourceFromURL(path)
    val data = source.mkString
    source.close()
    data
  }

  def readStrings(path: String, delimiter: String = ","): Seq[String] = {
    readResource(path).split(delimiter).map(_.trim)
  }  

  //Load the domain parameters (such as mean,stdev for parameters in the domain)
  def loadDomainParams(domainParamKBFile: String): Map[Param, Map[String, Double]] = {
    println(s"Loading domain parameters file from ${domainParamKBFile}")
    val source = Sourcer.sourceFromURL(domainParamKBFile)
    val domainParams = source
      .getLines //get all the lines
      .toArray
      .map{ line => // line = [param]\t[variable]\t[value] => e.g. "rainfall  mean  30.5"
        val fields = line.split("\t")
        val param = fields(0)
        val var_values = fields.tail
          .map{ var_value =>
            val tmp = var_value.split(":")
            val variable = tmp(0)
            val value = tmp(1).toDouble // Assuming the value of the variable is a double. TODO: Change this appropriately
            variable -> value
          }.toMap
        (param ->  var_values)
      }.toMap
    source.close

    println("loaded domain params:" + domainParams.toString())

    domainParams
  }

  //Load the gradable adjective grounding from file
  def loadGradableAdjGroundingFile(quantifierKBFile: String): Map[Quantifier, Map[String, Double]] = {
    println(s"Loading the gradable adjectives model file from ${quantifierKBFile}")
    // adjective -> Map(name:value)
    val source = Sourcer.sourceFromURL(quantifierKBFile)
    val gradableAdjModel = source
      .getLines //Get all the lines from the model KB file
      .toArray.tail // ignore the header line
      .map { line => // "adjective	mu_coefficient	sigma_coefficient	intercept"
      val fields = line.split("\t")
      val adj = fields(0)
      val mu_coeff = fields(1).toDouble
      val sigma_coeff = fields(2).toDouble
      val intercept = fields(3).toDouble
      adj -> Map(MU_COEFF -> mu_coeff, SIGMA_COEFF -> sigma_coeff, INTERCEPT -> intercept)
    }.toMap
    source.close
    gradableAdjModel
  }

  def loadWords(path: String, delimiter: String = ","): Seq[String] = {
    println(s"* Loading words from ${path}")
    val source = Sourcer.sourceFromURL(path)
    val data = source.getLines().toArray.filter(line => !line.startsWith("#")).mkString(" ")
    val words = data.split(delimiter).map(_.trim)
    source.close()
    words
  }

  def getCommentedTextFromFile(file: File, sep: String = " "): String = {
    println(s"Getting text from ${file.getName}")
    val source = Sourcer.sourceFromFile(file)
    
    try {
      source
          .getLines()
          .toArray
          .filter(line => !line.startsWith("#"))
          .mkString(sep)
    }
    finally {
      source.close()
    }
  }
  
  def getTextFromURL(path: String): String = {
    val source = Sourcer.sourceFromURL(path)
    
    try {
      source.mkString
    }
    finally {
      source.close()
    }
  }
  
  def getTextFromFile(file: File): String = {
    val source = Sourcer.sourceFromFile(file)
    
    try {
      source.mkString
    }
    finally {
      source.close()
    }
  }
  
  def loadYaml(path: String): Collection[Any] = {
    val input = getTextFromURL(path)
    val yaml = new Yaml(new Constructor(classOf[Collection[Any]]))
    
    yaml.load(input).asInstanceOf[Collection[Any]]
  }
}
