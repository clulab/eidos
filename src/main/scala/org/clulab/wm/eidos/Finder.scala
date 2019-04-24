package org.clulab.wm.eidos

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin.{Actions, ExtractorEngine, Mention}
import org.clulab.processors.{Document, Processor}
import org.clulab.wm.eidos.actions.CorefHandler
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.extraction.EidosReader
import org.clulab.wm.eidos.groundings.{OntologyGrounder, OntologyGrounding}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.DocumentFilter

import scala.util.matching.Regex

// -------------------------------
//            Whole System
// -------------------------------

// OR -- is this too much -- better handled in an App??
// todo
//class EidosSystem(reader: EidosReader, postProcessor: PostProcessor, serializer: EidosSerializer) {
//
//  def findFiles(s: String): Seq[String] = ??? // fixme, placeholder
//  def processDirectory(collectionDir: String, outputDir: String) = {
//    val annotated = for {
//      f <- findFiles(collectionDir)
//      text <- scala.io.Source.fromFile(f).getLines().toArray // fixme, placeholder
//      mentions = reader.extract(text)
//    } yield postProcessor.process(mentions)
//    serializer.serialize(annotated, outputDir)
//  }
//  // todo: better versions of input/output
//  def processFile(inputFile: String, outputFile: String) = {
//    ??? // maybe...?
//  }
//}



// todo
//class EidosActions(expander: Option[Expander], corefHandler: Option[CorefHandler]) extends Actions


// -------------------------------
//         Preprocessing
// -------------------------------

// outside -- in the system
trait PreProcessor {
  def parse(text: String): Document
}

// filter sentences and parse...
class EidosPreprocessor(documentFilter: Option[DocumentFilter], processor: Processor) extends PreProcessor {
  def parse(text: String): Document = ???
}


// -------------------------------
//            Expanders
// -------------------------------


trait Expander {
  def expand(ms: Seq[Mention]): Seq[Mention]
}

class TextBoundExpander(validLabels: Set[String], dependencies: Dependencies) extends Expander {
  def expand(ms: Seq[Mention]): Seq[Mention] = ???
}
object TextBoundExpander {
  def fromConfig(config: Config): TextBoundExpander = {
    ???
  }
}

class ArgumentExpander(validArgs: Set[String], validLabels: Set[String], dependencies: Dependencies) extends Expander {
  private val textBoundExpander = new TextBoundExpander(validLabels, dependencies)

  def expand(ms: Seq[Mention]): Seq[Mention] = ???
}
object ArgumentExpander {
  def fromConfig(config: Config): ArgumentExpander = {
    ???
  }
}
case class Dependencies(validIncoming: Set[Regex], invalidIncoming: Set[Regex], validOutgoing: Set[Regex], invalidOutgoing: Set[Regex])

object Expander {
  def fromConfig(config: Config): Expander = {
    val expandType: String = config[String]("expansionTyoe") // fixme
    expandType match {
      case "textbound" => TextBoundExpander.fromConfig(config) // todo: check about scoping with these nested configs
      case "argument" => ArgumentExpander.fromConfig(config)
      case _ => ???

    }
  }
}


// -------------------------------
//         ContentManagers
// -------------------------------





object EidosUtils {
  def isContentTag(tag: String): Boolean = tag.startsWith("NN") || tag.startsWith("VB")
}

// -------------------------------
//         PostProcessing
// -------------------------------

trait PostProcessingStep {
  def process(inputs:Seq[EidosMention]):Seq[EidosMention]
}

class PostProcessor(steps: Seq[PostProcessingStep]) {
  def toEidosMention(m: Mention): EidosMention = ???
  // todo: we should add `copy` methods to each type of EidosMention (and the super class) sot that we can rapidly
  // create the new ones with additional post processing

  def process(mentions: Seq[Mention]): AnnotatedDocument = {
    val eidosMentions = mentions.map(toEidosMention)

    // Apply all post-processing steps (e.g., canonicalizing, grounding, etc.)
    var postProcessedMentions = eidosMentions
    for(step <- steps) {
      postProcessedMentions = step.process(postProcessedMentions)
    }

    // Make empty doc
    val doc: Document = ???
    AnnotatedDocument(doc, mentions, postProcessedMentions)
  }


}

// Filter and canonicalize
class CanonicalizerStep(stopWords: Set[String], transparentWords: Set[String], stopNER: Set[String]) extends PostProcessingStep {
  // Make the canonical name
  def process(inputs:Seq[EidosMention]):Seq[EidosMention] = ???

  def isContent(s: String): Boolean = !isStop(s) && !isTransparent(s)
  def isStop(s:String): Boolean = stopWords.contains(s)
  def isTransparent(s: String): Boolean = transparentWords.contains(s)

  def isCanonical(lemma: String, tag: String, ner: String): Boolean = {
    // Valid POS, not a stop/transparent word, and not a named entity we're choosing to ignore
    // todo: lemma for isContent?  if so, let's rename the method or its args
    EidosUtils.isContentTag(tag) && isContent(lemma) && !stopNER.contains(ner)
  }

}

class GroundingStep(ontologies: Seq[OntologyGrounder]) extends PostProcessingStep {
  // Add the grounding, based on the canonical name
  def process(inputs:Seq[EidosMention]):Seq[EidosMention] = ???
}


//trait ddd {
//  val isPrimary: Boolean
//
//  def groundOntology(mention: EidosMention, previousGroundings: Option[Map[String, OntologyGrounding]]): OntologyGrounding
//  def groundOntology(mention: EidosMention): OntologyGrounding = groundOntology(mention, None)
//  def groundOntology(mention: EidosMention, previousGroundings: Map[String, OntologyGrounding]): OntologyGrounding = groundOntology(mention, Some(previousGroundings))
//
//  def groundable(mention: EidosMention, previousGroundings: Option[Map[String, OntologyGrounding]]): Boolean
//  def groundable(mention: EidosMention): Boolean = groundable(mention, None)
//  def groundable(mention: EidosMention, previousGroundings: Map[String, OntologyGrounding]): Boolean = groundable(mention, Some(previousGroundings))
//
//}


// -------------------------------
//            Exporting
// -------------------------------



trait EidosSerializer {
  // and some other versions for File, etc.
  def serialize(annotatedDocuments: Seq[AnnotatedDocument], filename: String): Unit
}

class JLDSerializer extends EidosSerializer {
  override def serialize(annotatedDocuments: Seq[AnnotatedDocument], filename: String): Unit = ???
}
// For the vanilla odin format used in PT influence search
class OdinClassicInfluenceSerializer extends EidosSerializer {
  override def serialize(annotatedDocuments: Seq[AnnotatedDocument], filename: String): Unit = ???
}