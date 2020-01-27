package org.clulab.wm.eidos.apps

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.EidosOntologyGrounder
import org.clulab.wm.eidos.groundings.TreeDomainOntology
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.Sinker
import org.clulab.wm.eidos.utils.Sourcer
import org.clulab.wm.eidos.utils.TsvUtils.TsvReader
import org.clulab.wm.eidos.utils.TsvUtils.TsvWriter

object EvalOntologyGrounders extends App {

  class Grounder {
    val name = "wm"
    protected val ontologyGrounder: EidosOntologyGrounder =
      new EidosSystem().components.ontologyHandler.ontologyGrounders.find(_.name == name).get

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
  }

  val inputFile = args(0)
  val outputFile = args(1)
  val grounder = new Grounder()
  val reader = new TsvReader()

  Sourcer.sourceFromFile(inputFile).autoClose { source =>
    Sinker.printWriterFromFile(outputFile).autoClose { printWriter =>
      val writer = new TsvWriter(printWriter)

      writer.println("TEXT", "START", "END", "GROUNDING")
      source.getLines.drop(1).foreach { line =>
        val Array(text, start, end, grounding) = reader.readln(line)
        val nameAndValueAndIsLeafOpt: Option[(String, Float, Boolean)] = grounder.ground(canonicalName)
        val (name, value, isLeaf) = nameAndValueAndIsLeafOpt
            .map { case (name, value, isLeaf) => (name, value.toString, if (isLeaf) "T" else "F") }
            .getOrElse(("", "", ""))

        writer.println(file, id, text, canonicalName, isLeaf, value, name)
      }
    }
  }
}

