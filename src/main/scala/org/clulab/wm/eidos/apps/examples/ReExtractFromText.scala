package org.clulab.wm.eidos.apps.examples

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.OntologyHandler
import org.clulab.wm.eidos.groundings.grounders.EidosOntologyGrounder
import org.clulab.wm.eidos.serialization.jsonld.JLDCorpus
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.StringUtils
import org.clulab.wm.ontologies.NodeTreeDomainOntologyBuilder

object ReExtractFromText extends App {

  def replaceCompositionalOntology(oldReader: EidosSystem, compositionalOntology: String): EidosSystem = {
    val oldComponents = oldReader.components
    val oldTokenizer = oldComponents.procOpt.get.getTokenizer
    val oldOntologyHandler = oldComponents.ontologyHandlerOpt.get
    val oldSentencesExtractor = oldOntologyHandler.sentencesExtractor
    val oldCanonicalizer = oldOntologyHandler.canonicalizer

    val newDomainOntologyBuilder = new NodeTreeDomainOntologyBuilder(oldSentencesExtractor, oldCanonicalizer, filtered = true)
    val newDomainOntology = FileUtils.newFileInputStream(compositionalOntology).autoClose { inputStream =>
      newDomainOntologyBuilder.buildFromStream(inputStream, Some(StringUtils.afterLast(compositionalOntology, '/')), None)
    }
    val newOntologyGrounder = EidosOntologyGrounder.mkGrounder(EidosOntologyGrounder.PRIMARY_NAMESPACE, newDomainOntology, oldOntologyHandler.wordToVec, oldOntologyHandler.canonicalizer, oldTokenizer)
    val newOntologyGrounders = oldOntologyHandler.ontologyGrounders.map { ontologyGrounder =>
      if (ontologyGrounder.name == EidosOntologyGrounder.PRIMARY_NAMESPACE) newOntologyGrounder
      else ontologyGrounder
    }
    val newOntologyHandler = new OntologyHandler(
      newOntologyGrounders,
      oldOntologyHandler.wordToVec,
      oldOntologyHandler.sentencesExtractor,
      oldOntologyHandler.canonicalizer,
      oldOntologyHandler.includeParents,
      oldOntologyHandler.topN,
      oldOntologyHandler.threshold
    )
    val newComponents = oldComponents.copy(ontologyHandlerOpt = Some(newOntologyHandler))
    val newReader = new EidosSystem(newComponents)

    newReader
  }

  val text = "Floods cause migration. Migration causes conflict."
  val reader = new EidosSystem()
  reader.extractFromText(text)

  val oldCompositionalOntology = "./ontology_before.yml"
  val oldReader = replaceCompositionalOntology(reader, oldCompositionalOntology)
  val oldPath = oldCompositionalOntology + ".jsonld"

  val newCompositionalOntology = "./ontology_after.yml"
  val newReader = replaceCompositionalOntology(reader, newCompositionalOntology)
  val newPath = newCompositionalOntology + ".jsonld"

  def extract(reader: EidosSystem, path: String, regrounding: Boolean): Unit = {
    val annotatedDocument = reader.extractFromText(text)

    FileUtils.printWriterFromFile(path).autoClose { printWriter =>
      new JLDCorpus(annotatedDocument).serialize(printWriter, regrounding = regrounding)
    }
  }

  extract(oldReader, oldPath, regrounding = false)
  extract(newReader, newPath, regrounding = true)
}
