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
import org.clulab.wm.eidos.utils.{DocumentFilter}
import org.clulab.wm.eidos.utils.StringUtils.toRegex

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
//         ContentManagers
// -------------------------------

object EidosUtils {
  def isContentTag(tag: String): Boolean = tag.startsWith("NN") || tag.startsWith("VB")
}

// -------------------------------
//         PostProcessing
// -------------------------------



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