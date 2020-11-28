package org.clulab.wm.eidos.apps

import ai.lum.common.ConfigUtils._
import org.clulab.struct.Interval
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.{IndividualGrounding, OntologyHandler}
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sourcer
import org.clulab.wm.eidos.utils.StringUtils
import org.clulab.wm.eidos.utils.TsvReader
import org.clulab.wm.eidos.utils.TsvWriter

// This app creates a spreadsheet to be filled in with gold groundings.

object GenerateGoldGroundingTSV extends App {

  class Evaluator(eidosSystem: EidosSystem) {
    protected val ontologyHandler: OntologyHandler = eidosSystem.components.ontologyHandlerOpt.get

    def evaluate(causeOrEffect: String, sentence: String): (String, Seq[(String, String)]) = {
      val start = sentence.indexOf(causeOrEffect)
      val end = start + causeOrEffect.length
      val namesAndScores: Seq[(String, String)] = if (start >= 0) {
        val document = eidosSystem.annotate(sentence)
        val interval = Interval(start, end)
        val allGroundings = ontologyHandler.reground(sentence, interval, document)

        Evaluator.grounderNames.map { groundingName =>
          allGroundings(groundingName)
              .headOption
              .map { case g: IndividualGrounding =>
                (g.name, g.score.toString)
              }
              .getOrElse(("None", "None"))
        }
      }
      else
        Seq.empty

      (s"($start,$end)", namesAndScores)
    }
  }

  object Evaluator {
    val grounderNames = Seq(
      "wm_flattened",
      "wm_compositional/concept",
      "wm_compositional/process",
      "wm_compositional/property"
    )
  }

  // Load tsv files from resources directory (as files, not as resources).
  val directory = "src/main/resources/org/clulab/wm/eidos/english/grounding/"
  val  inFilename = directory + "groundingEvalEntities.tsv"
  val outFilename = directory + "gold_groundings.tsv"
  val errFilename = directory + "rejected_sentences.tsv"
  val headers = Array (
    /*  0 */ "GOLD Annotated?",
    /*  1 */ "Index",
    /*  2 */ "Sentence",
    /*  3 */ "Entity",
    /*  4 */ "Character Offsets",
    /*  5 */ "Cause/Effect",
    /*  6 */ "GOLD Flat Grounding",
    /*  7 */ "GOLD Flat Grounding Score",
    /*  8 */ "GOLD Concept Grounding",
    /*  9 */ "GOLD Concept Score",
    /* 10 */ "GOLD Process Grounding",
    /* 11 */ "GOLD Process Score",
    /* 12 */ "GOLD Property Grounding",
    /* 13 */ "GOLD Property Score"
  )
  val config = EidosSystem.defaultConfig
  val useGrounding = config[Boolean]("ontologies.useGrounding")
  val grounderNames: List[String] = config[List[String]]("ontologies.ontologies")

  assert(useGrounding, "Grounding is required for this app.")
  Evaluator.grounderNames.foreach { grounderName =>
    assert(grounderNames.contains(StringUtils.beforeFirst(grounderName, '/')),
      s"Ontology for $grounderName must be configured.")
  }

  val eidosSystem = new EidosSystem(config)
  val evaluator = new Evaluator(eidosSystem)
  val tsvReader = new TsvReader()

  new TsvWriter(FileUtils.printWriterFromFile(outFilename), isExcel = false).autoClose { outTsvWriter =>
    new TsvWriter(FileUtils.printWriterFromFile(errFilename), isExcel = false).autoClose { errTsvWriter =>
      outTsvWriter.println(headers)
      errTsvWriter.println(headers)
      Sourcer.sourceFromFile(inFilename).autoClose { source =>
        source.getLines.drop(1).foreach { line =>
          val Array(
            /*  0 */ index,
            /*  1 */ _,
            /*  2 */ _,
            /*  3 */ _,
            /*  4 */ cause,
            /*  5 */ _,
            /*  6 */ _,
            /*  7 */ _,
            /*  8 */ _,
            /*  9 */ _,
            /* 10 */ _,
            /* 11 */ effect,
            /* 12 */ _,
            /* 13 */ _,
            /* 14 */ _,
            /* 15 */ _,
            /* 16 */ _,
            /* 17 */ _,
            /* 18 */ sentence
          ) = tsvReader.readln(line)

          Seq(("cause", cause), ("effect", effect)).foreach { case (label, value) =>
            val (interval, groundings) = new Evaluator(eidosSystem).evaluate(value, sentence)

            if (groundings.nonEmpty) {
              outTsvWriter.println(
                /*  0 */ "",
                /*  1 */ index,
                /*  2 */ sentence.trim(),
                /*  3 */ value,
                /*  4 */ interval,
                /*  5 */ label,
                /*  6 */ groundings(0)._1,
                /*  7 */ groundings(0)._2,
                /*  8 */ groundings(1)._1,
                /*  9 */ groundings(1)._2,
                /* 10 */ groundings(2)._1,
                /* 11 */ groundings(2)._2,
                /* 12 */ groundings(3)._1,
                /* 13 */ groundings(3)._2
              )
            }
            else
              errTsvWriter.printWriter.print(line + "\n")
          }
        }
      }
    }
  }
}
