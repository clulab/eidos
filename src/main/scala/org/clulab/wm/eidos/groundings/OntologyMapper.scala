package org.clulab.wm.eidos.groundings

import ai.lum.common.ConfigUtils._
import org.clulab.embeddings.DefaultWordSanitizer
import org.clulab.wm.eidos.EidosConfigured
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.grounders.EidosOntologyGrounder
import org.clulab.wm.eidos.groundings.grounders.FlatOntologyGrounder
import org.clulab.wm.eidoscommon.Canonicalizer
import org.clulab.wm.eidoscommon.EidosProcessor
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.PassThruNamer
import org.clulab.wm.eidoscommon.utils.Sourcer
import org.clulab.wm.ontologies.DomainOntology

import scala.collection.mutable.ArrayBuffer

object OntologyMapper extends EidosConfigured {
  val sanitizer: DefaultWordSanitizer = EidosWordToVec.sanitizer

  def loadOtherOntology(file: String, w2v: EidosWordToVec): Seq[ConceptEmbedding] = {
    val ces = Sourcer.sourceFromFile(file).autoClose { source =>
      val lines = source.getLines()
      val result = for {
        line <- lines
        fields = line.split("\t")
        path = fields(0).split(",")
        examples = fields(1).split(",").map(sanitizer.sanitizeWord(_))
        embedding = w2v.makeCompositeVector(examples)
      } yield ConceptEmbedding(new PassThruNamer(path.mkString(DomainOntology.SEPARATOR)), embedding)

      result.toArray
    }
    ces
  }

  // todo: do I need to split on underscore -- you betcha!
  def getParents(path: String): Seq[String] = {
    path.split(DomainOntology.SEPARATOR) //.flatMap(elem => elem.split("[ |_]"))
  }

  def replaceSofiaAbbrev(str: String): String = {
    if (str == "manag") "management"
    else if (str == "cogn") "cognitive"
    else str
  }

  // Filter out non-content words or not
  def selectWords(ws: Seq[String], contentOnly: Boolean, proc: EidosProcessor): Seq[String] = {
    val tagSet = proc.getTagSet

    if (contentOnly) {
      val doc = proc.mkDocument(ws.mkString(" "))
      proc.tagPartsOfSpeech(doc)
      for {
        (w, i) <- doc.sentences.head.words.zipWithIndex
        tag = doc.sentences.head.tags.get(i)
        if tagSet.isOntologyContent(tag) && w != "provision"
      } yield w
    }
    else {
      ws
    }
  }

  // todo query expansion
  def mweStringSimilarity(a: String, b: String, reader: EidosSystem): Float = {
    val eidosWordToVec = reader.components.ontologyHandlerOpt.get.wordToVec

    def mkMWEmbedding(s: String): Array[Float] = {
      val words = s.split("[ |_]").map(sanitizer.sanitizeWord(_)).map(replaceSofiaAbbrev)
      eidosWordToVec.makeCompositeVector(selectWords(words, contentOnly = false, reader.components.procOpt.get))
    }

    EidosWordToVec.dotProduct(mkMWEmbedding(a), mkMWEmbedding(b))
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

  def pairwiseScore(ce1: ConceptEmbedding, ce2: ConceptEmbedding, reader: EidosSystem, exampleWeight: Float, parentWeight: Float): Float = {
    // Semantic similarity between leaf nodes (based on their examples)
    val examplesScore = EidosWordToVec.dotProduct(ce1.embedding, ce2.embedding)
    // Semantic similarity of the parents (going up the hierarchy, more weight closer to leaves)
    val structureScore = weightedParentScore(ce1.namer.getName, ce2.namer.getName, reader)
    // Similarity between the indicator an the ontology concept ancestors
    //val indicatorToAncestorScore = weightedNodeToParentScore(ce1.namer.name, ce2.namer.name, reader)

    exampleWeight * examplesScore + parentWeight * structureScore //+ 0.3 * indicatorToAncestorScore
  }

  def mostSimilarIndicators(concepts: Seq[ConceptEmbedding], indicators: Seq[ConceptEmbedding], n: Int = 10,
                            reader: EidosSystem, exampleWeight: Float = 0.8f, parentWeight: Float = 0.1f): Seq[(String, Seq[(String, Float)])] = {
    concepts.map(c => (c.namer.getName, mostSimilar(c, indicators, n, reader)))
  }

  // n is to limit the number returned, 0 means return all
  def mostSimilar(concept: ConceptEmbedding, indicators: Seq[ConceptEmbedding], n: Int, reader: EidosSystem, exampleWeight: Float = 0.8f, parentWeight: Float = 0.1f): Seq[(String, Float)] = {
    println(s"comparing ${concept.namer.getName}...")
    val comparisons = indicators.map(indicator => (indicator.namer.getName, pairwiseScore(concept, indicator, reader, exampleWeight, parentWeight))) //.filter(p => p._2 > 0.7)
    if (n > 0) {
      comparisons.sortBy(-_._2).take(n)
    } else {
      comparisons.sortBy(-_._2)
    }
  }

  def eidosGrounders(reader: EidosSystem): Seq[EidosOntologyGrounder] = {
    reader.components.ontologyHandlerOpt.get.ontologyGrounders.collect {
      case e: FlatOntologyGrounder => e
    }
  }

  def mapIndicators(reader: EidosSystem): Unit = {
    // Update the indicator mapping file
    val outputFile = config[String]("apps.ontologyMapper.outfile")
    val topN = config[Int]("apps.groundTopN")
    mapIndicators(reader, outputFile, topN)
  }

  // Mapping the primary ontology to the indicator ontologies
  def mapIndicators(reader: EidosSystem, outputFile: String, topN: Int): Unit = {
    // FIXME: should this be the base grounder or the compositional one??
    val grounders: Seq[EidosOntologyGrounder] = eidosGrounders(reader)
    println(s"number of eidos ontologies - ${grounders.length}")
    // For purposes of this app, it is assumed that the primary grounder exists.
    val primaryGrounder = grounders.find { grounder => grounder.name == EidosOntologyGrounder.PRIMARY_NAMESPACE }.get
    val primaryConceptEmbeddings = primaryGrounder.conceptEmbeddings
    val primaryKeys = primaryConceptEmbeddings.map(_.namer.getName)
    val indicatorGrounders = grounders.filter(g => EidosOntologyGrounder.indicatorNamespaces.contains(g.name))
    val indicatorMaps = indicatorGrounders.map { ontology: EidosOntologyGrounder =>
      val namespace = ontology.name
      val concepts = ontology.conceptEmbeddings
      val mostSimilar: Map[String, Seq[(String, Float)]] = mostSimilarIndicators(primaryConceptEmbeddings, concepts, topN, reader).toMap

      mostSimilar.foreach(mapping => println(s"primary: ${mapping._1} --> most similar $namespace: ${mapping._2.mkString(",")}"))
      (namespace, mostSimilar)
    }
    //    val primaryIndicatorMap = indicatorMaps.find { indicatorMap => indicatorMap._1 == EidosOntologyGrounder.PRIMARY_NAMESPACE }

    // Write the mapping file
    FileUtils.printWriterFromFile(outputFile).autoClose { pw =>
      FileUtils.printWriterFromFile(outputFile + ".no_ind_for_interventions").autoClose { pwInterventionSpecific =>

        for (primaryConcept <- primaryKeys) {
          val mappings = indicatorMaps.flatMap(x => x._2(primaryConcept).map(p => (p._1, p._2, x._1)))
          val sorted = mappings.sortBy(-_._2)

          for ((indicator, score, label) <- sorted) {
            pw.println(s"primaryConcept\t$primaryConcept\t$label\t$indicator\t$score")
            if (!primaryConcept.startsWith("wm/concept/causal_factor/intervention")) { // Check the file for how this is named!
              pwInterventionSpecific.println(s"primaryConcept\t$primaryConcept\t$label\t$indicator\t$score")
            }
          }
        }
      }
    }
  }

  /** Generates the mappings between the reader ontologies in a String format
    *
    * @param reader           EidosSystem
    * @param sofiaPath        path to the Sofia ontology file
    * @param providedOntology a YAML string representing the ontology against which mappings are generated
    * @param providedOntName  name of the ontology provided in the providedOntology argument
    * @param exampleWeight    the weight for the similarity of the pair of nodes (default = 0.8)
    * @param parentWeight     the weight for the similarity of the node parents (default = 0.1)
    * @param topN             the number of similarity scores to return, when set to 0, return them all (default = 0)
    * @return String version of the mapping, akin to a file, newlines separate the "rows"
    */
  def mapOntologies(reader: EidosSystem, sofiaPath: String, providedOntology: Option[String], providedOntName: String = "ProvidedOntology",
                    exampleWeight: Float = 0.8f, parentWeight: Float = 0.1f, topN: Int = 0): String = {
    // EidosSystem stuff
    val proc = reader.components.procOpt.get
    val w2v = reader.components.ontologyHandlerOpt.get.wordToVec
    val canonicalizer = reader.components.ontologyHandlerOpt.get.canonicalizer
    val includeParents = reader.components.ontologyHandlerOpt.get.includeParents
    val tagSet = reader.components.procOpt.get.getTagSet

    // Load
    val eidosConceptEmbeddings = if (providedOntology.nonEmpty) {
      val ontology = OntologyHandler.mkDomainOntologyFromYaml(providedOntName, providedOntology.get, proc, new Canonicalizer(reader.components.stopwordManagerOpt.get, tagSet), includeParents = includeParents)
      val grounder = EidosOntologyGrounder(providedOntName, ontology, w2v, canonicalizer)
      grounder.conceptEmbeddings
    } else {
      // FIXME: How do we know what the head is?
      eidosGrounders(reader).head.conceptEmbeddings
    }
    val sofiaConceptEmbeddings = loadOtherOntology(sofiaPath, w2v)
    println("Finished loading other ontologies")

    // Initialize output
    val sb = new ArrayBuffer[String]()

    val eidos2Sofia = mostSimilarIndicators(eidosConceptEmbeddings, sofiaConceptEmbeddings, topN, reader, exampleWeight, parentWeight)

    for {
      (eidosConcept, sofiaMappings) <- eidos2Sofia
      (sofiaConcept, score) <- sofiaMappings
    } sb += s"$providedOntName\t$eidosConcept\tSOFIA\t$sofiaConcept\t$score"

    sb.mkString("\n")
  }
}
