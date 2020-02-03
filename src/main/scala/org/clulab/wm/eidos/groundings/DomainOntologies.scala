package org.clulab.wm.eidos.groundings

import java.time.ZonedDateTime

import com.github.clulab.eidos.Version
import com.github.clulab.eidos.Versions
import com.github.worldModelers.ontologies.{Version => AwayVersion}
import com.github.worldModelers.ontologies.{Versions => AwayVersions}
import org.clulab.wm.eidos.SentencesExtractor
import org.clulab.wm.eidos.groundings.OntologyHandler.serializedPath
import org.clulab.wm.eidos.groundings.HalfTreeDomainOntology.HalfTreeDomainOntologyBuilder
import org.clulab.wm.eidos.utils.Canonicalizer
import org.clulab.wm.eidos.utils.StringUtils
import org.slf4j.Logger
import org.slf4j.LoggerFactory

object DomainOntologies {
  protected lazy val logger: Logger = LoggerFactory.getLogger(getClass)

  // The intention is to stop the proliferation of the generated Version class to this single method.
  protected def getVersionOpt(ontologyPath: String): (Option[String], Option[ZonedDateTime]) = {
    // This should work for local ontologies.  Absolute
    val goodVersionOpt = Versions.versions.get(MockVersions.codeDir + ontologyPath)
    // See what might have come from WordModelers/Ontologies
    val bestVersionOpt = goodVersionOpt.getOrElse {
      // These are always stored in top level directory.
      val awayVersionOpt = AwayVersions.versions.get(StringUtils.afterLast(ontologyPath, '/')).getOrElse(None)
      val homeVersionOpt = awayVersionOpt.map { awayVersion => Version(awayVersion.commit, awayVersion.date) }

      homeVersionOpt
    }

    if (bestVersionOpt.isDefined)
      (Some(bestVersionOpt.get.commit), Some(bestVersionOpt.get.date))
    else
      (None, None)
  }

  def apply(ontologyPath: String, serializedPath: String, sentencesExtractor: SentencesExtractor, canonicalizer: Canonicalizer, filter: Boolean = true, useCache: Boolean = false, includeParents: Boolean = false): DomainOntology = {

    // We're skipping the cache for experiments
//    if (useCache) {
//      logger.info(s"Processing cached yml ontology $serializedPath...")
//      CompactDomainOntology.load(serializedPath)
//    }
//    else {
      logger.info(s"Processing yml ontology $ontologyPath...")
      val (versionOpt, dateOpt) = getVersionOpt(ontologyPath)
// kwa do something with includeParents
      new HalfTreeDomainOntologyBuilder(sentencesExtractor, canonicalizer, filter).buildFromPath(ontologyPath, versionOpt, dateOpt)
//    }
  }

  def mkDomainOntology(name: String, ontologyPath: String, sentenceExtractor: SentencesExtractor, canonicalizer: Canonicalizer, cacheDir: String, useCached: Boolean): DomainOntology = {
    val ontSerializedPath: String = serializedPath(name, cacheDir)
    DomainOntologies(ontologyPath, ontSerializedPath, sentenceExtractor, canonicalizer: Canonicalizer, filter = true, useCache = useCached)
  }
}
