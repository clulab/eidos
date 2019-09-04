package org.clulab.wm.eidos.apps

import ai.lum.common.ConfigUtils._
import com.typesafe.config.ConfigFactory
import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.processors.Processor
import org.clulab.wm.eidos.utils.{Canonicalizer, FileUtils, PassThruNamer, Sourcer}
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.utils.Closer.AutoCloser

import scala.collection.mutable.ArrayBuffer

object OntologyMapper {

  // All of this and the call to mapIndicators is usually arranged in CacheOntologies.
  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load("eidos")
    val outputFile = config[String]("apps.ontologymapper.outfile")
    val topN = config[Int]("apps.groundTopN")
    val reader = new EidosSystem(config)

    mapIndicators(reader, outputFile, topN)
  }

  def loadOtherOntology(file: String, w2v: EidosWordToVec): Seq[ConceptEmbedding] = {
    val ces = Sourcer.sourceFromFile(file).autoClose { source =>
      val lines = source.getLines()
      val result = for {
        line <- lines
        fields = line.split("\t")
        path = fields(0).split(",")
        //pathSanitized = path.map(Word2Vec.sanitizeWord(_))
        examples = fields(1).split(",").map(Word2Vec.sanitizeWord(_))
        embedding = w2v.makeCompositeVector(examples)
      } yield ConceptEmbedding(new PassThruNamer(path.mkString(DomainOntology.SEPARATOR)), embedding)

      result.toArray
    }
    ces
  }

  // todo: do I need to split on underscore -- you betcha!
  def getParents(path: String): Seq[String] = {
    path.split(DomainOntology.SEPARATOR)//.flatMap(elem => elem.split("[ |_]"))
  }

  def replaceSofiaAbbrev(str: String): String = {
    if (str == "manag") "management"
    else if (str == "cogn") "cognitive"
    else str
  }

  // Filter out non-content words or not
  def selectWords(ws: Seq[String], contentOnly: Boolean, proc: Processor): Seq[String] = {

    def contentTag(s: String): Boolean = {
      s.startsWith("N") || s.startsWith("V") || s.startsWith("J")
    }

    if (contentOnly) {
      val doc = proc.mkDocument(ws.mkString(" "))
      proc.tagPartsOfSpeech(doc)
      for {
        (w, i) <- doc.sentences.head.words.zipWithIndex
        tag = doc.sentences.head.tags.get(i)
        if contentTag(tag) && w != "provision"
      } yield w
    }
    else {
      ws
    }
  }

  // todo query expansion

  def mkMWEmbedding(s: String, reader: EidosSystem, contentOnly: Boolean = false): Array[Float] = {
    val words = s.split("[ |_]").map(Word2Vec.sanitizeWord(_)).map(replaceSofiaAbbrev)
    reader.components.ontologyHandler.wordToVec.makeCompositeVector(selectWords(words, contentOnly, reader.components.proc))
  }

  def mweStringSimilarity(a: String, b: String, reader: EidosSystem): Float = {
    dotProduct(mkMWEmbedding(a, reader), mkMWEmbedding(b, reader))
  }

  def weightedParentScore(path1: String, path2: String, reader: EidosSystem): Float = {
    // like entity/natural_resource/water ==> Seq(entity, natural_resource, water)
    // reverse: Seq(water, natural_resource, water)
    val parents1 = getParents(path1).reverse
    val parents2 = getParents(path2).reverse
    val k = math.min(parents1.length, parents2.length)
    var avg = 0.0f // optimization
    for (i <- 0 until k) {
      val score = mweStringSimilarity(parents1(i), parents2(i), reader)
      avg += score / (i + 1)
    }
    avg
  }

  def weightedNodeToParentScore(conceptPath: String, indicatorPath: String, reader: EidosSystem): Double = {
    // like entity/natural_resource/water ==> Seq(entity, natural_resource, water)
    // reverse: Seq(water, natural_resource, water)
    val conceptParents = getParents(conceptPath).reverse
    val indicator = getParents(indicatorPath).reverse.head

    var avg: Double = 0.0 // optimization
    for (i <- 1 until conceptParents.length) {
      val score = mweStringSimilarity(conceptParents(i), indicator, reader)
      avg += score / i
    }
    avg
  }

  // In case one day we want to expand the concept with synonyms
  def expandedConcept(path: String): Seq[String] = {
    val conceptString = getParents(path).last
    ???
  }

  def dotProduct(v1:Array[Float], v2:Array[Float]):Float = {
    assert(v1.length == v2.length) //should we always assume that v2 is longer? perhaps set shorter to length of longer...
    var sum = 0.0f // optimization
    var i = 0 // optimization
    while(i < v1.length) {
      sum += v1(i) * v2(i)
      i += 1
    }
    sum
  }

  def pairwiseScore(ce1: ConceptEmbedding, ce2: ConceptEmbedding, reader: EidosSystem, exampleWeight: Float, parentWeight: Float): Float = {
    // Semantic similarity between leaf nodes (based on their examples)
    val examplesScore = dotProduct(ce1.embedding, ce2.embedding)
    // Semantic similarity of the parents (going up the hierarchy, more weight closer to leaves)
    val structureScore = weightedParentScore(ce1.namer.name, ce2.namer.name, reader)
    // Similarity between the indicator an the ontology concept ancestors
    //val indicatorToAncestorScore = weightedNodeToParentScore(ce1.namer.name, ce2.namer.name, reader)

    exampleWeight * examplesScore + parentWeight * structureScore //+ 0.3 * indicatorToAncestorScore
  }

  def mostSimilarIndicators(concepts: Seq[ConceptEmbedding], indicators: Seq[ConceptEmbedding], n: Int = 10,
                            reader: EidosSystem, exampleWeight: Float = 0.8f, parentWeight: Float = 0.1f): Seq[(String, Seq[(String, Float)])] = {
    concepts.map(c => (c.namer.name, mostSimilar(c, indicators, n, reader)))
  }

  // n is to limit the number returned, 0 means return all
  def mostSimilar(concept: ConceptEmbedding, indicators: Seq[ConceptEmbedding], n: Int, reader: EidosSystem, exampleWeight: Float = 0.8f, parentWeight: Float = 0.1f): Seq[(String, Float)] = {
    println(s"comparing $concept...")
    val comparisons = indicators.map(indicator => (indicator.namer.name, pairwiseScore(concept, indicator, reader, exampleWeight, parentWeight)))//.filter(p => p._2 > 0.7)
    if (n > 0) {
      comparisons.sortBy(- _._2).take(n)
    } else {
      comparisons.sortBy(- _._2)
    }
  }

  // Mapping the primary ontology to the indicator ontologies
  def mapIndicators(reader: EidosSystem, outputFile: String, topN: Int): Unit = {
    val grounders: Seq[EidosOntologyGrounder] = reader.components.ontologyHandler.grounders
    println(s"number of eidos ontologies - ${grounders.length}")
    // For purposes of this app, it is assumed that the primary grounder exists.
    val primaryGrounder = grounders.find { grounder => grounder.name == EidosOntologyGrounder.PRIMARY_NAMESPACE }.get
    val primaryConceptEmbeddings = primaryGrounder.conceptEmbeddings
    val indicatorMaps = grounders.map { ontology: EidosOntologyGrounder =>
      val namespace = ontology.name
      val concepts = ontology.conceptEmbeddings
      val mostSimilar: Map[String, Seq[(String, Float)]] = mostSimilarIndicators(primaryConceptEmbeddings, concepts, topN, reader).toMap

      mostSimilar.foreach(mapping => println(s"primary: ${mapping._1} --> most similar $namespace: ${mapping._2.mkString(",")}"))
      (namespace, mostSimilar)
    }
    val primaryIndicatorMap = indicatorMaps.find { indicatorMap => indicatorMap._1 == EidosOntologyGrounder.PRIMARY_NAMESPACE }.get

    // Write the mapping file
    FileUtils.printWriterFromFile(outputFile).autoClose { pw =>
      FileUtils.printWriterFromFile(outputFile + ".no_ind_for_interventions").autoClose { pwInterventionSpecific =>

        for (primaryConcept <- primaryIndicatorMap._2.keys) {
          val mappings = indicatorMaps.flatMap(x => x._2(primaryConcept).map(p => (p._1, p._2, x._1)))
          val sorted = mappings.sortBy(-_._2)
          
          for ((indicator, score, label) <- sorted) {
            pw.println(s"primaryConcept\t$primaryConcept\t$label\t$indicator\t$score")
            if (!primaryConcept.startsWith("wm/intervention")) { // Check the file for how this is named!
              pwInterventionSpecific.println(s"primaryConcept\t$primaryConcept\t$label\t$indicator\t$score")
            }
          }
        }

        // These would probably need to be changed from un to primary.
        // Back when we were doing an exhaustive mapping...
        //  for {
        //    (unConcept, indicatorMappings) <- un2fao
        //    (faoIndicator, score) <- indicatorMappings
        //  } pw.println(s"unConcept\t$unConcept\tFAO\t$faoIndicator\t$score")
        //
        //  for {
        //    (unConcept, indicatorMappings) <- un2wdi
        //    (wdiIndicator, score) <- indicatorMappings
        //  } pw.println(s"unConcept\t$unConcept\tWB\t$wdiIndicator\t$score")
        //
      }
    }
  }

  /** Generates the mappings between the reader ontologies in a String format
    *
    * @param reader EidosSystem
    * @param bbnPath path to the BBN ontology file
    * @param sofiaPath path to the Sofia ontology file
    * @param exampleWeight the weight for the similarity of the pair of nodes (default = 0.8)
    * @param parentWeight the weight for the similarity of the node parents (default = 0.1)
    * @param topN the number of similarity scores to return, when set to 0, return them all (default = 0)
    * @return String version of the mapping, akin to a file, newlines separate the "rows"
    */
  def mapOntologies(reader: EidosSystem, bbnPath: String, sofiaPath: String, providedOntology: Option[String], providedOntName: String = "ProvidedOntology",
                    exampleWeight: Float = 0.8f, parentWeight: Float = 0.1f, topN: Int = 0): String = {
    // EidosSystem stuff
    val proc = reader.components.proc
    val w2v = reader.components.ontologyHandler.wordToVec
//    val config = reader.config

    // Load
    val eidosConceptEmbeddings = if (providedOntology.nonEmpty) {
      val grounder = OntologyHandler.mkDomainOntologyFromYaml(providedOntName, providedOntology.get, proc, new Canonicalizer(reader.components.stopwordManager))
      grounder match {
        case g: EidosOntologyGrounder => g.conceptEmbeddings
        case _ => throw new RuntimeException("Custom ontology Grounder must be an EidosOntologyGrounder")
      }
    } else {

      // TODO.  How do we know what the head is?
      reader.components.ontologyHandler.grounders.head.conceptEmbeddings
    }
    val sofiaConceptEmbeddings = loadOtherOntology(sofiaPath, w2v)
    val bbnConceptEmbeddings = loadOtherOntology(bbnPath, w2v)
    println("Finished loading other ontologies")

    // Initialize output
    val sb = new ArrayBuffer[String]()

    val eidos2Sofia = mostSimilarIndicators(eidosConceptEmbeddings, sofiaConceptEmbeddings, topN, reader, exampleWeight, parentWeight)
    val eidos2BBN = mostSimilarIndicators(eidosConceptEmbeddings, bbnConceptEmbeddings, topN, reader, exampleWeight, parentWeight)
    val sofia2BBN = mostSimilarIndicators(sofiaConceptEmbeddings, bbnConceptEmbeddings, topN, reader, exampleWeight, parentWeight)

    for {
      (eidosConcept, sofiaMappings) <- eidos2Sofia
      (sofiaConcept, score) <- sofiaMappings
    } sb += s"primary\t$eidosConcept\tSOFIA\t$sofiaConcept\t$score"

    for {
      (eidosConcept, bbnMappings) <- eidos2BBN
      (bbnConcept, score) <- bbnMappings
    } sb += s"primary\t$eidosConcept\tHUME\t$bbnConcept\t$score"

    for {
      (sofiaConcept, bbnMappings) <- sofia2BBN
      (bbnConcept, score) <- bbnMappings
    } sb += s"SOFIA\t$sofiaConcept\tHUME\t$bbnConcept\t$score"

    sb.mkString("\n")
  }
}
