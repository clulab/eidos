package org.clulab.wm.eidos.apps

import ai.lum.common.ConfigUtils._
import org.clulab.struct.Interval
import org.clulab.struct.MutableNumber
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.OntologyAliases.OntologyGroundings
import org.clulab.wm.eidos.groundings.OntologyHandler
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.Counter
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sourcer
import org.clulab.wm.eidos.utils.StringUtils
import org.clulab.wm.eidos.utils.TsvReader
import org.clulab.wm.eidos.utils.TsvWriter

// This app reads in a tsv file with gold groundings, regrounds the entities,
// and compares with the gold groundings.

object EvalGroundings extends App {

  class Evaluator(description: String, val grounderName: String) {
    val separator = ", "
    val none = "None"
    val correct = new MutableNumber[Float](0)
    val counter: Counter = Counter()

    def getCount: String = counter.get.toString

    def getAccuracy: String = description + " grounding accuracy:\t" +
        (correct.value / counter.get).toString

    // Get top 5 groundings (as strings) to compare with gold groundings.
    protected def getTop5(allGroundings: OntologyGroundings): Seq[String] =
        allGroundings(grounderName)
          .take(5)
          .map(_._1.name)

    def evaluate(allGroundings: OntologyGroundings, gold: String): (String, String) = {
      val top5 = getTop5(allGroundings)
      val (top, isCorrect, score) = if (top5.isEmpty)
        (none, gold == none, if (gold == none) 1f else 0f)
      else {
        val golds = gold.split(separator)
        // Find the first of the top values that is in the gold values, even if a value
        // further down in the top values may have a higher index in the gold.
        val top5Index = top5.indexWhere(golds.contains(_))

        if (top5Index >= 0)
          (top5(top5Index), true, 1f / (top5Index + 1))  // Add MRR score based on position in golds.
        else
          (top5.mkString(separator), false, 0f) // This is a fake top for diagnostic purposes.
      }

      counter.inc()
      correct.value += score
      (top, isCorrect.toString)
    }
  }

  val directory = "src/main/resources/org/clulab/wm/eidos/english/grounding/"
  val  inFilename = directory + "gold_groundings_annotated.tsv"
  val outFilename = directory + "evaluation_results.tsv"

  val evaluators = Array(
    new Evaluator("flat",     "wm_flattened"),
    new Evaluator("concept",  "wm_compositional/concept"),
    new Evaluator("process",  "wm_compositional/process"),
    new Evaluator("property", "wm_compositional/property")
  )

  val config = EidosSystem.defaultConfig
  val useGrounding = config[Boolean]("ontologies.useGrounding")
  val grounderNames: List[String] = config[List[String]]("ontologies.ontologies")

  assert(useGrounding, "Grounding is required for this app.")
  evaluators.foreach { evaluator =>
    assert(grounderNames.contains(StringUtils.beforeFirst(evaluator.grounderName, '/')),
        s"{$evaluator.grounderName} must be configured.")
  }

  val eidosSystem = new EidosSystem(config)
  val ontologyHandler: OntologyHandler = eidosSystem.components.ontologyHandler
  val tsvReader = new TsvReader()

  new TsvWriter(FileUtils.printWriterFromFile(outFilename), isExcel = false).autoClose { tsvWriter =>
    tsvWriter.println(
      /*  0 */ "Index",
      /*  1 */ "Sentence",
      /*  2 */ "Entity",
      /*  3 */ "Character Offsets",
      /*  4 */ "cause/effect",
      /*  5 */ "GOLD Flat Grounding",
      /*  6 */ "Current Flat Grounding",
      /*  7 */ "Flat Correct?",
      /*  8 */ "GOLD Concept Grounding",
      /*  9 */ "Current Concept Grounding",
      /* 10 */ "Concept Correct?",
      /* 11 */ "GOLD Process Grounding",
      /* 12 */ "Current Process Grounding",
      /* 13 */ "Process Correct?",
      /* 14 */ "GOLD Property Grounding",
      /* 15 */ "Current Property Grounding",
      /* 16 */ "Property Correct?",
      /* 17 */ "Notes", "" // It had an extra.
    )

    Sourcer.sourceFromFile(inFilename).autoClose { sourcer =>
      // Loop over each line in tsv file (except header line).
      sourcer.getLines.drop(1).foreach { line =>
        val Array( // cells from original tsv
          /*  0 */ annotated,
          /*  1 */ index,
          /*  2 */ sentence,
          /*  3 */ entity,
          /*  4 */ offsetsText,
          /*  5 */ causeOrEffect,
          /*  6 */ flatGold,
          /*  7 */ _,
          /*  8 */ conceptGold,
          /*  9 */ _,
          /* 10 */ processGold,
          /* 11 */ _,
          /* 12 */ propertyGold
        ) = tsvReader.readln(line, 13) // The incoming file is jagged.

        // Only do the comparison if the gold entity has been annotated (e.g. made sure the gold grounding is correct).
        if (annotated == "y") {
          // Remove leading ( and trailing ) before splitting.
          val Array(start, end) = offsetsText.slice(1, offsetsText.length() - 1).split(',')
          val interval = Interval(start.toInt, end.toInt)
          // Make a Document out of the sentence.
          val document = eidosSystem.annotate(sentence)
          // Get all groundings for the entity.
          val allGroundings: OntologyGroundings = ontologyHandler.reground(sentence, interval, document)
          val golds = Array(flatGold, conceptGold, processGold, propertyGold)
          val returnsAndCorrects = evaluators.zip(golds).map { case (evaluator, gold) =>
            evaluator.evaluate(allGroundings, gold)
          }

          tsvWriter.println(
            /*  0 */ index,
            /*  1 */ sentence,
            /*  2 */ entity,
            /*  3 */ offsetsText,
            /*  4 */ causeOrEffect,
            /*  5 */ golds(0),
            /*  6 */ returnsAndCorrects(0)._1,
            /*  7 */ returnsAndCorrects(0)._2,
            /*  8 */ golds(1),
            /*  9 */ returnsAndCorrects(1)._1,
            /* 10 */ returnsAndCorrects(1)._2,
            /* 11 */ golds(2),
            /* 12 */ returnsAndCorrects(2)._1,
            /* 13 */ returnsAndCorrects(2)._2,
            /* 14 */ golds(3),
            /* 15 */ returnsAndCorrects(3)._1,
            /* 16 */ returnsAndCorrects(3)._2
          )
        }
      }
    }
    println(evaluators(0).getCount + " entities evaluated")
    evaluators.foreach { evaluator => println(evaluator.getAccuracy) }
  }
}
