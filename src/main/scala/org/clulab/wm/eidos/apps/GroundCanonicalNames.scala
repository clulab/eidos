package org.clulab.wm.eidos.apps

import org.clulab.struct.Interval
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.{OntologyGrounder, OntologyHandler, PredicateGrounding, SingleOntologyNodeGrounding}
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.Sinker
import org.clulab.wm.eidoscommon.utils.Sourcer
import org.clulab.wm.eidoscommon.utils.{TsvReader, TsvWriter, XsvUtils}

object GroundCanonicalNames extends App {

  class Grounder {
    val name = "wm"
    protected val ontologyHandler: OntologyHandler = new EidosSystem().components.ontologyHandlerOpt.get
    protected val ontologyGrounder: OntologyGrounder = ontologyHandler.ontologyGrounders.find(_.name == name).get
    protected val nameToIsLeaf: Map[String, Boolean] = {
      val domainOntology = ontologyGrounder.domainOntology
      domainOntology.indices.map { index =>
        val name = domainOntology.getNamer(index).name
        val isLeaf = domainOntology.isLeaf(index)
        name -> isLeaf
      }.toMap
    }

    def split(text: String): Array[String] = text.split(' ')

    protected def topGroundingNameAndValue(strings: Array[String]): Option[(String, Float)] = {
      val allGroundings = ontologyGrounder.groundStrings(strings)
      val namerAndFloatOpt = allGroundings.head.headOption
      val nameAndValueOpt = namerAndFloatOpt.map {
        case g: SingleOntologyNodeGrounding => (g.name, g.score)
        case pred: PredicateGrounding => ???
      }

      nameAndValueOpt
    }

    def ground(canonicalName: String): Option[(String, Float, Boolean)] = {
      val nameAndValueOpt = topGroundingNameAndValue(split(canonicalName))

      nameAndValueOpt.map { case (name, value) =>
        (name, value, nameToIsLeaf(name))
      }
    }

    // This method is not actually used in the program.  It only attempts to find the start and stop
    // of the canonicalName in the text in order to exercise/demonstrate the reground functionality of
    // the ontologyHandler (when some of the code below is changed).  Normally the ground method
    // above is used and that only for experimentation itself.
    def ground(text: String, canonicalName: String): Option[(String, Float, Boolean)] = {
      val canonicalNameParts = canonicalName.split(' ')
      val start = text.toLowerCase.indexOf(canonicalNameParts.head.toLowerCase)
      val stop = text.toLowerCase.indexOf(canonicalNameParts.last.toLowerCase, start)
      if (start >= 0 && stop >= 0) {
        val groundings = ontologyHandler.reground(text, Interval(start, stop + canonicalNameParts.last.length))

        if (groundings.nonEmpty && groundings.head._2.nonEmpty) {
          val individualGrounding = groundings.head._2.headOption.get

          Some((individualGrounding.name, individualGrounding.score, false))
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
    new TsvWriter(Sinker.printWriterFromFile(outputFile)).autoClose { tsvWriter =>
      val tsvReader = new TsvReader()

      tsvWriter.println("file", "id", "text", "canonicalName", "isLeaf", "score", "grounding")
      source.getLines.drop(1).foreach { line =>
        val Array(file, id, text, canonicalName) = tsvReader.readln(line)
        val nameAndValueAndIsLeafOpt: Option[(String, Float, Boolean)] = grounder.ground(canonicalName)
        val (name, value, isLeaf) = nameAndValueAndIsLeafOpt
            .map { case (name, value, isLeaf) => (name, value.toString, if (isLeaf) "T" else "F") }
            .getOrElse(("", "", ""))

        tsvWriter.println(file, id, text, canonicalName, isLeaf, value, name)
      }
    }
  }
}
