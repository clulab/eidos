package org.clulab.wm.eidos.apps

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.EidosOntologyGrounder
import org.clulab.wm.eidos.groundings.OntologyGrounding
import org.clulab.wm.eidos.groundings.TreeDomainOntology
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.Namer
import org.clulab.wm.eidos.utils.Sinker
import org.clulab.wm.eidos.utils.Sourcer

object GroundCanonicalNames extends App {
  type SingleOntologyGrounding = (Namer, Float)
  type MultipleOntologyGrounding = Seq[SingleOntologyGrounding]
  type OntologyGroundings = Map[String, OntologyGrounding]

  def escape(text: String): String = {
    text
        .replace("\\", "\\\\")
        .replace("\n", "\\n")
        .replace("\r", "\\r")
        .replace("\t", "\\t")
  }

  def unescape(text: String): String = {
    text
        .replace("\\t", "\t")
        .replace("\\r", "\r")
        .replace("\\n", "\n")
        .replace("\\\\", "\\")
  }

  class Grounder {
    val name = "wm"
    protected val ontologyGrounder: EidosOntologyGrounder =
        new EidosSystem().components.ontologyHandler.grounders.find (_.name == name).get
    protected val nameToIsLeaf: Map[String, Boolean] = {
      val domainOntology = ontologyGrounder.domainOntology
      val treeDomainOntology = {
        if (!domainOntology.isInstanceOf[TreeDomainOntology])
          throw new RuntimeException("I need a TreeDomainOntology, which is only possible if cached ontologies are _not_ used!")
        domainOntology.asInstanceOf[TreeDomainOntology]
      }
      val size = treeDomainOntology.size

      0.until(size).map { index =>
        val node = treeDomainOntology.getNode(index)

        node.fullName -> node.isLeaf
      }.toMap
    }

    def split(text: String): Array[String] = text.split(' ')

    protected def topGroundingName(strings: Array[String]): Option[String] = {
      val allGroundings = ontologyGrounder.groundStrings(strings)
      val topGroundingNameOpt = allGroundings.head.headName

      topGroundingNameOpt
    }

    def ground(canonicalName: String): Option[(String, Boolean)] = {
      val nameOpt = topGroundingName(split(canonicalName))

      nameOpt.map { name =>
        (name, nameToIsLeaf(name))
      }
    }
  }

  val inputFile = args(0)
  val outputFile = args(1)
  val grounder = new Grounder()

  Sourcer.sourceFromFile(inputFile).autoClose { source =>
    Sinker.printWriterFromFile(outputFile).autoClose { printWriter =>
      printWriter.println("file\tid\ttext\tcanonicalName\tisLeaf\tgrounding")
      source.getLines.drop(1).foreach { line =>
        val Array(file, id, text, escapedCanonicalName) = line.split('\t')
        val canonicalName = unescape(escapedCanonicalName)
        val nameAndIsLeafOpt: Option[(String, Boolean)] = grounder.ground(canonicalName)
        val (name, isLeaf) = nameAndIsLeafOpt.map { case (name, isLeaf) => (name, if (isLeaf) "T" else "F") }.getOrElse(("", ""))

        printWriter.println(s"$file\t$id\t$text\t$escapedCanonicalName\t${escape(isLeaf)}\t${escape(name)}")
      }
    }
  }
}
