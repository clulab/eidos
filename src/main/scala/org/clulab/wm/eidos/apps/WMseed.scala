package org.clulab.wm.eidos.apps

import java.io.File

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging

import org.clulab.processors.{Document, Processor}
import ai.lum.common.ConfigUtils._
import org.clulab.odin.Mention
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.utils.DisplayUtils.displayMentions

/**
  * Created by ajaynagesh on 5/31/17.
  */
object WMseed extends App with LazyLogging {

  val config = ConfigFactory.load()         // load the configuration file

  val entitiesMasterFile = config[String]("wmseed.entitiesMasterFile")
  val eventsMasterFile = config[String]("wmseed.eventsMasterFile")
  val quantifierKB: String = config[String]("wmseed.quantifierKB")
  val documentPath = config[String]("wmseed.documentPath")

  // print the parameters
  logger.info(s"Entities master file : $entitiesMasterFile")
  logger.info(s"Events master file : $eventsMasterFile")
  logger.info(s"Quantifier KB : $quantifierKB")
  logger.info(s"Document Path : $documentPath")

  // creates an extractor engine using the rules and the default actions
  val extractor = new EidosSystem() // masterRulesPath = entitiesMasterFile)
  // maybe new EidosSystem(ConfigFactory.load("wmseedopen").getConfig("Eidos"))

  // create the processor
  val proc: Processor = extractor.proc

  // List of files to be processed
  val fileList = getListOfFiles(documentPath)

  logger.info(s"$fileList")

  for(file <- fileList){
    logger.info(s"Processing file ${file.getName}")
    val text = scala.io.Source.fromFile(file).getLines().mkString("\n")

    // process text
    extractFrom(text)

  }

  def extractFrom(text:String): Unit = {
    // preprocessing
    logger.info(s"Text : $text")
    val doc = proc.annotate(text)

    // extract mentions from annotated document
    val mentions = extractor.extractFrom(doc).sortBy(m => (m.sentence, m.getClass.getSimpleName))

    // debug display the mentions
    displayMentions(mentions, doc)

    // pretty display
    prettyDisplay(mentions, doc)
  }

  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  def prettyDisplay(mentions:Seq[Mention], doc:Document): Unit = {
    val events = mentions.filter(_ matches "Event")
    val params = new mutable.HashMap[String, ListBuffer[(String, String)]]()
    for(e <- events) {
      val f = formal(e)
      if(f.isDefined) {
        val just = e.text
        val sent = e.sentenceObj.getSentenceText
        val quantifier = e.arguments.get("quantifier")
        params.getOrElseUpdate(f.get, new ListBuffer[(String, String)]) += new Tuple2(just, sent)
      }
    }

    if(params.nonEmpty) {
      println("Eidos Parameters:")
      for (k <- params.keySet) {
        val evidence = params.get(k).get
        println(s"$k: ${evidence.size} instances:")
        for (e <- evidence) {
          println(s"\tJustification: [${e._1}]")
          println(s"""\tSentence: "${e._2}"""")
        }
        println()
      }
    }
  }

  def formal(e:Mention):Option[String] = {
    var t = ""
    if(e matches "Decrease") t = "DECREASE"
    else if(e matches "Increase") t = "INCREASE"
    else return None

    Some(s"$t of ${e.arguments.get("theme").get.head.label}")
  }

}
