package org.clulab.wm.eidos.groundings

import java.time.ZonedDateTime
import com.github.clulab.eidos.Version
import com.github.clulab.eidos.Versions
import com.github.worldModelers.ontologies.{Versions => AwayVersions}
import org.clulab.wm.eidos.groundings.OntologyHandler.serializedPath
import org.clulab.wm.eidoscommon.Canonicalizer
import org.clulab.wm.eidoscommon.SentencesExtractor
import org.clulab.wm.eidoscommon.utils.Logging
import org.clulab.wm.eidoscommon.utils.StringUtils
import org.clulab.wm.ontologies.FullTreeDomainOntology.FullTreeDomainOntologyBuilder
import org.clulab.wm.ontologies.{CompactDomainOntology, DomainOntology, FastDomainOntology}
import org.clulab.wm.ontologies.HalfTreeDomainOntology.HalfTreeDomainOntologyBuilder

object DomainHandler extends Logging {

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

  def apply(ontologyPath: String, serializedPath: String, sentencesExtractor: SentencesExtractor,
      canonicalizer: Canonicalizer, filter: Boolean = true, useCacheForOntologies: Boolean = false,
      includeParents: Boolean = false): DomainOntology = {

    // As coded below, when parents are included, the FullTreeDomainOntology is being used.
    // The faster loading version is the FastDomainOntology.
    // If parents are not included, as had traditionally been the case, the HalfTreeDomainOntology suffices.
    // Being smaller and faster, it is preferred.  The faster loading counterpart is CompactDomainOntology.
    if (includeParents) {
      if (useCacheForOntologies) {
        logger.info(s"Processing cached yml ontology with parents from $serializedPath...")
        FastDomainOntology.load(serializedPath)
      }
      else {
        logger.info(s"Processing yml ontology with parents from $ontologyPath...")
        val (versionOpt, dateOpt) = getVersionOpt(ontologyPath)
        new FullTreeDomainOntologyBuilder(sentencesExtractor, canonicalizer, filter).buildFromPath(ontologyPath, versionOpt, dateOpt)
      }
    }
    else {
      if (useCacheForOntologies) {
        logger.info(s"Processing cached yml ontology without parents from $serializedPath...")
        CompactDomainOntology.load(serializedPath)
      }
      else {
        logger.info(s"Processing yml ontology without parents from $ontologyPath...")
        val (versionOpt, dateOpt) = getVersionOpt(ontologyPath)
        new HalfTreeDomainOntologyBuilder(sentencesExtractor, canonicalizer, filter).buildFromPath(ontologyPath, versionOpt, dateOpt)
      }
    }
  }

  def mkDomainOntology(name: String, ontologyPath: String, sentenceExtractor: SentencesExtractor,
      canonicalizer: Canonicalizer, cacheDir: String, useCacheForOntologies: Boolean,
      includeParents: Boolean): DomainOntology = {
    val ontSerializedPath: String = serializedPath(name, cacheDir, includeParents)

    DomainHandler(ontologyPath, ontSerializedPath, sentenceExtractor, canonicalizer: Canonicalizer, filter = true,
        useCacheForOntologies = useCacheForOntologies, includeParents = includeParents)
  }
}
