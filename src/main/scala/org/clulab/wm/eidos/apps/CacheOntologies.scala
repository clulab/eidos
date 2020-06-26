package org.clulab.wm.eidos.apps

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import ai.lum.common.ConfigUtils._
import com.typesafe.config.ConfigFactory
import org.clulab.embeddings.CompactWordEmbeddingMap
import org.clulab.geonorm.GeoNamesIndex
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.ontologies.CompactDomainOntology.CompactDomainOntologyBuilder
import org.clulab.wm.eidos.groundings.ontologies.FastDomainOntology.FastDomainOntologyBuilder
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.groundings.ontologies.FullTreeDomainOntology
import org.clulab.wm.eidos.groundings.ontologies.HalfTreeDomainOntology
import org.clulab.wm.eidos.utils.Domain

object CacheOntologies extends App {
  val config = EidosSystem.defaultConfig
  val includeParents: Boolean = config[Boolean]("ontologies.includeParents")
  val cacheDir: String = config[String]("ontologies.cacheDir")
  // Not all operations require the reader, so hedge your bets.
  lazy val reader = new EidosSystem(config)

  new File(cacheDir).mkdirs()

  def deleteFiles(path: Path): Unit = {
    if (Files.exists(path)) {
      // This is written for Scala 1.11 compatibility.
      Files.list(path).toArray.foreach { path =>
        Files.deleteIfExists(path.asInstanceOf[Path])
      }
      // At least on Windows, the directory doesn't disappear until the application exists.
      // In the meantime, it will be locked and unavailable for use by GeoNamesIndex.
      // However, it is is left around but empty, it works OK.  So, skip this.
      // Files.deleteIfExists(path)
    }
  }

  def createFiles(path: Path): Unit = {
    GeoNamesIndex.fromClasspathJar(path)
  }

  def replaceGeoNorms(): Unit = {
    val geoNamesDir: Path = Paths.get(config[String]("geonorm.geoNamesDir")).toAbsolutePath.normalize

    deleteFiles(geoNamesDir)
    createFiles(geoNamesDir)
  }

  def cacheOntologies(): Unit = {
    val ontologyGrounders: Seq[OntologyGrounder] = reader.components.ontologyHandler.ontologyGrounders

    if (ontologyGrounders.isEmpty)
      throw new RuntimeException("No ontologies were specified, please check the config file.")
    else {
      if (includeParents) {
        println(s"Saving full ontologies to $cacheDir...")
        ontologyGrounders.foreach { grounder =>
          val ontologyName = grounder.name
          val ontology: DomainOntology = grounder.domainOntology
          // convert
          val treeDomainOntology = ontology.asInstanceOf[FullTreeDomainOntology]
          val fastDomainOntology = new FastDomainOntologyBuilder(treeDomainOntology).build()
          // save
          val serializedPath = OntologyHandler.serializedPath(ontologyName, cacheDir, includeParents)
          fastDomainOntology.save(serializedPath)
        }
      }
      else {
        println(s"Saving half ontologies to $cacheDir...")
        ontologyGrounders.foreach { grounder =>
          val ontologyName = grounder.name
          val ontology: DomainOntology = grounder.domainOntology
          // convert
          val treeDomainOntology = ontology.asInstanceOf[HalfTreeDomainOntology]
          val compactDomainOntology = new CompactDomainOntologyBuilder(treeDomainOntology).build()
          // save
          val serializedPath = OntologyHandler.serializedPath(ontologyName, cacheDir, includeParents)
          compactDomainOntology.save(serializedPath)
        }
      }
      println(s"Finished serializing ${ontologyGrounders.length} ontologies.")
    }
  }

  def cacheWord2Vec(): Unit = {
    val filenameIn = config[String]("ontologies.wordToVecPath")
    val filenameOut = EidosWordToVec.makeCachedFilename(cacheDir, filenameIn)
    println(s"Saving vectors to $filenameOut...")
    val word2Vec = reader.components.ontologyHandler.wordToVec match {
      case realWordToVec: RealWordToVec =>
        if (!config[Boolean]("ontologies.useCacheForW2V"))
          realWordToVec.w2v // It wasn't cached, so we must have an up-to-date version.
        else
          CompactWordEmbeddingMap(filenameIn, resource = true, cached = false)
      case _ =>  CompactWordEmbeddingMap(filenameIn, resource = true, cached = false)
    }
    word2Vec.save(filenameOut)
    println(s"Finished serializing vectors.")
  }

  def updateIndicatorMappings(): Unit = {
    // Update the indicator mapping file
    val outputFile = config[String]("apps.ontologymapper.outfile")
    val topN = config[Int]("apps.groundTopN")
    OntologyMapper.mapIndicators(reader, outputFile, topN)
  }

  def safeCacheOntologies(): Unit = {
    // When not grounding, neither ontologies nor vectors should be loaded at all.  Require grounding, thus.
    assert(config[Boolean]("ontologies.useGrounding") == true, "To use CacheOntologies, you must set useGrounding = true")
    // Since here we want to cache the current versions, we can't load from cached in case they aren't current.
    assert(config[Boolean]("ontologies.useCacheForOntologies") == false, "To use CacheOntologies, you must set ontologies.useCacheForOntologies = false")
    // Relax this following requirement.  We often recache the vectors, even if the data has not changed.
    // This strategy may change in important ways if we use the ontologies during the filtering of the vectors.
    // assert(config[Boolean]("ontologies.useCacheForW2V") == true, "To use CacheOntologies, you must set useCacheForW2V = false")

    cacheOntologies()
  }

  // Comment these in and out as required.
  replaceGeoNorms() // This should go first before EidosSystem is created.
//  cacheWord2Vec()
//  safeCacheOntologies()
//  updateIndicatorMappings()
}
