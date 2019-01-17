package org.clulab.wm.eidos.apps

import java.io.File

import ai.lum.common.ConfigUtils._
import com.typesafe.config.ConfigFactory
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.CompactDomainOntology.CompactDomainOntologyBuilder
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.utils.Canonicalizer

object CacheOntologies extends App {

  val config = ConfigFactory.load("eidos")
  val reader = new EidosSystem(config)
  val cacheDir: String = reader.LoadableAttributes.cacheDir
  // Since here we want to cache the current, we can't load from cached:
  assert(config[Boolean]("ontologies.useCache") == false, "To use CacheOntologies, you must set ontologies.useCache = false")
  assert(reader.LoadableAttributes.useW2V == true, "To use CacheOntologies, you must set useW2V = true")
  new File(cacheDir).mkdirs()

  val ontologyGrounders: Seq[EidosOntologyGrounder] = reader.loadableAttributes.ontologyGrounders

  if (ontologyGrounders.isEmpty)
    throw new RuntimeException("No ontologies were specified, please check the config file.")
  else {
    val proc = reader.proc

    println(s"Saving ontologies to $cacheDir...")
    ontologyGrounders.foreach { grounder =>
      val ontologyName = grounder.name
      val ontology: DomainOntology = grounder.domainOntology
      // convert
      val treeDomainOntology = ontology.asInstanceOf[TreeDomainOntology]
      val compactDomainOntology = new CompactDomainOntologyBuilder(treeDomainOntology).build()
      // save
      val serializedPath = OntologyHandler.serializedPath(ontologyName, cacheDir)
      compactDomainOntology.save(serializedPath)
    }
    println(s"Finished serializing ${ontologyGrounders.length} ontologies.")
  }

  val filenameIn = reader.LoadableAttributes.wordToVecPath
  val filenameOut = EidosWordToVec.makeCachedFilename(cacheDir, filenameIn)
  println(s"Saving vectors to $filenameOut...")
  val word2Vec = CompactWord2Vec(filenameIn, resource = true, cached = false)
  word2Vec.save(filenameOut)
  println(s"Finished serializing vectors.")

  // Update the indicator mapping file
  val outputFile = config[String]("apps.ontologymapper.outfile")
  val topN = config[Int]("apps.groundTopN")
  OntologyMapper.mapIndicators(reader, outputFile, topN)
}
