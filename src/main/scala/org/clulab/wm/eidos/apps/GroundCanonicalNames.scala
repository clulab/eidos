package org.clulab.wm.eidos.apps

import org.clulab.struct.Interval
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.OntologyGrounder
import org.clulab.wm.eidos.groundings.OntologyHandler
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.Sinker
import org.clulab.wm.eidos.utils.Sourcer
import org.clulab.wm.eidos.utils.TsvUtils
import org.clulab.wm.eidos.utils.TsvUtils.TsvWriter

object GroundCanonicalNames extends App {

  class Grounder {
    val name = "wm"
    protected val ontologyHandler: OntologyHandler = new EidosSystem().components.ontologyHandler
    protected val ontologyGrounder: OntologyGrounder = ontologyHandler.ontologyGrounders.find(_.name == name).get
    protected val nameToIsLeaf: Map[String, Boolean] = {
      val domainOntology = ontologyGrounder.domainOntology
      0.until(domainOntology.size).map { index =>
        val name = domainOntology.getNamer(index).name
        val isLeaf = domainOntology.isLeaf(index)
        name -> isLeaf
      }.toMap
    }

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
        (name, value, nameToIsLeaf(name))
      }
    }

    def ground(text: String, canonicalName: String): Option[(String, Float, Boolean)] = {
      val canonicalNameParts = canonicalName.split(' ')
      val start = text.toLowerCase.indexOf(canonicalNameParts.head.toLowerCase)
      val stop = text.toLowerCase.indexOf(canonicalNameParts.last.toLowerCase, start)
      if (start >= 0 && stop >= 0) {
        val groundings = ontologyHandler.reground(text, Interval(start, stop + canonicalNameParts.last.length))

        if (groundings.nonEmpty && groundings.head._2.nonEmpty) {
          val (namer, value) = groundings.head._2.headOption.get

          Some((namer.name, value, false))
        }
        else None
      }
      else
        None
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
