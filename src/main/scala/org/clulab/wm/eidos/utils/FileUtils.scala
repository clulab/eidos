package org.clulab.wm.eidos.utils

import java.io.{File, FilenameFilter}
import java.util.jar.JarFile
import org.clulab.wm.EidosSystem
import org.clulab.wm.EidosSystem.{INTERCEPT, MU_COEFF, SIGMA_COEFF}
import scala.collection.mutable.ListBuffer
import org.clulab.wm.EidosSystem

object FileUtils {

  import org.clulab.wm.eidos.Aliases._

  def readRules(rulesPath: String): String = {
    println(s"rulesPath:\t$rulesPath")
    val source = scala.io.Source.fromURL(getClass.getResource(rulesPath))
    val rules = source.mkString
    source.close()
    rules
  }

  //Load the domain parameters (such as mean,stdev for parameters in the domain)
  def loadDomainParams(domainParamKBFile: String): Map[Param, Map[String, Double]] = {
    println(s"Loading domain parameters file from ${domainParamKBFile}")
    val source = scala.io.Source.fromURL(getClass.getResource(domainParamKBFile))
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
    val source = scala.io.Source.fromURL(getClass.getResource(quantifierKBFile))
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

  // Get all files in a directory ending with a given extension
  def findFilesFromResources(resourcesPath: String, fileExtension:String): Seq[String] = {
    //    val sdsdj = new File(resourcesPath).list()
    //    sdsdj.foreach(println)
    //    println ("resourcesPath: " + resourcesPath)
    val jarFile = new File(getClass.getProtectionDomain.getCodeSource.getLocation.getPath)
    if (jarFile.isFile()) {
      val filenames = new ListBuffer[String]
      val jarEntries = new JarFile(jarFile).entries()

      while (jarEntries.hasMoreElements) {
        val nextEntry = jarEntries.nextElement().getName
        if (nextEntry.startsWith(resourcesPath + "/") && nextEntry.endsWith(fileExtension)) {
          //        println("next entry name: " + nextEntry)
          filenames.append(s"${nextEntry}")
        }

      }
      filenames.toList
    } else {
      val url = classOf[EidosSystem].getResource("/" + resourcesPath)
      val apps = new File(url.toURI)

      for (app <- apps.listFiles) {
        System.out.println(app)
      }
      val filenames = apps.listFiles.map { file =>
        // get absolute path
        val absolutePath = file.getAbsolutePath
        // find the index of where the relative path starts
        val locationOfResources = absolutePath.indexOf("org/clulab/wm")
        // get slice from there on
        absolutePath.slice(locationOfResources, absolutePath.length)
      }

      filenames.toSeq

    }
  }

  def findFiles(collectionDir: String, extension: String): Seq[File] = {
    val dir = new File(collectionDir)
    dir.listFiles(new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name.endsWith(extension)
    })
  }

}
