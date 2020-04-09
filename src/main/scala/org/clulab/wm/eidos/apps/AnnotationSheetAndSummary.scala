package org.clulab.wm.eidos.apps


import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.utils.Configured
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.apps.ReconstituteAndExport.getArgString
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.serialization.json.JLDDeserializer
import org.clulab.wm.eidos.utils.{FileUtils, ThreadUtils}

object AnnotationSheetAndSummary extends App with Configured {

  collection.parallel.ForkJoinTasks.defaultForkJoinPool
  lazy val reader = new EidosSystem()
  lazy val deserializer = new JLDDeserializer()
  val inputDir = getArgString("apps.inputDirectory", None)
  val outputDir = getArgString("apps.outputDirectory", None)
  val inputExtension = getArgString("apps.inputFileExtension", None)
  val nCores = getArgInt("apps.nCores", Some(1))

  // handle text or previously generated jsonld
  def getInput(f: File): AnnotatedDocument = {
    inputExtension match {
      case j if j.endsWith("jsonld") =>
        val json = FileUtils.getTextFromFile(f)
        deserializer.deserialize(json).head
      case _ =>
        val text = FileUtils.getTextFromFile(f)
        reader.extractFromText(text)
    }
  }

  val config = ConfigFactory.load("eidos")
  override def getConf: Config = config

  val files = FileUtils.findFiles(inputDir, inputExtension)
  val parFiles = ThreadUtils.parallelize(files, nCores)
  val annotatedDocuments = parFiles.map(getInput)

  



}
