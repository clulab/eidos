package org.clulab.wm.eidos.apps

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.OntologyGrounder
import org.clulab.wm.eidos.groundings.HalfTreeDomainOntology
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.Sinker
import org.clulab.wm.eidos.utils.Sourcer
import org.clulab.wm.eidos.utils.TsvUtils
import org.clulab.wm.eidos.utils.TsvUtils.TsvWriter

object GroundCanonicalNames extends App {

  class Grounder {
    val name = "wm"
    protected val ontologyGrounder: OntologyGrounder =
        new EidosSystem().components.ontologyHandler.ontologyGrounders.find(_.name == name).get
//    protected val nameToIsLeaf: Map[String, Boolean] = {
//      val domainOntology = ontologyGrounder.domainOntology
//      val treeDomainOntology = {
//        if (!domainOntology.isInstanceOf[TreeDomainOntology])
//          throw new RuntimeException("I need a TreeDomainOntology, which is only possible if cached ontologies are _not_ used!")
//        domainOntology.asInstanceOf[TreeDomainOntology]
//      }
//
//      treeDomainOntology.ontologyNodes.map { ontologyNode =>
//        ontologyNode.fullName -> false // ontologyNode.isLeaf
//      }.toMap
//    }

    def split(text: String): Array[String] = text.split(' ')

    protected def topGroundingNameAndValue(strings: Array[String]): Option[(String, Float)] = {
      val allGroundings = ontologyGrounder.groundStrings(strings)
      val namerAndFloatOpt = allGroundings.head.headOption
      val nameAndValueOpt = namerAndFloatOpt.map { case (namer, value) => (namer.name, value) }

      nameAndValueOpt
    }

    def ground(canonicalName: String): Option[(String, Float, Boolean)] = {
      val nameAndValueOpt = topGroundingNameAndValue(split(canonicalName))

      nameAndValueOpt.map { case (name, value) =>
        (name, value, false) // nameToIsLeaf(name))
      }
    }
  }

  val inputFile = args(0)
  val outputFile = args(1)
  val grounder = new Grounder()

  Sourcer.sourceFromFile(inputFile).autoClose { source =>
    Sinker.printWriterFromFile(outputFile).autoClose { printWriter =>
      val writer = new TsvWriter(printWriter)

      writer.println("file", "id", "text", "canonicalName", "isLeaf", "score", "grounding")
      source.getLines.drop(1).foreach { line =>
        val Array(file, id, text, canonicalName) = TsvUtils.readln(line)
        val nameAndValueAndIsLeafOpt: Option[(String, Float, Boolean)] = grounder.ground(canonicalName)
        val (name, value, isLeaf) = nameAndValueAndIsLeafOpt
            .map { case (name, value, isLeaf) => (name, value.toString, if (isLeaf) "T" else "F") }
            .getOrElse(("", "", ""))

        writer.println(file, id, text, canonicalName, isLeaf, value, name)
      }
    }
  }
}
