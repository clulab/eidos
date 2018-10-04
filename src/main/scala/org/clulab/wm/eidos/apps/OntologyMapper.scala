package org.clulab.wm.eidos.apps

import java.io.PrintWriter

import ai.lum.common.ConfigUtils._
import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.wm.eidos.utils.PassThruNamer
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.{ConceptEmbedding, DomainOntology, OntologyNode}
import org.clulab.wm.eidos.utils.FileUtils.printWriterFromFile

object OntologyMapper extends App {

  val reader = new EidosSystem()
  val proc = reader.proc
  val w2v = reader.wordToVec

  val config = reader.config

  val outputFile: String = config[String]("apps.ontologymapper.outfile")
  val topN: Int = config[Int]("apps.groundTopN")

  // Do something...
  mapIndicators(reader, outputFile, topN)


  def loadOtherOntology(file: String): Seq[ConceptEmbedding] = {
    val source = scala.io.Source.fromFile(file)
    val lines = source.getLines().toSeq
    val ces = for {
      line <- lines
      fields  = line.split("\t")
      path = fields(0).split(",")
      //pathSanitized = path.map(Word2Vec.sanitizeWord(_))
      examples = fields(1).split(",").map(Word2Vec.sanitizeWord(_))
      embedding = reader.wordToVec.makeCompositeVector(examples)
    } yield new ConceptEmbedding(new PassThruNamer(path.mkString(DomainOntology.SEPARATOR)), embedding)

    //source.close()

    ces
  }

  // todo: do I need to split on underscore -- you betcha!
  def getParents(path: String): Seq[String] = {
    path.split(DomainOntology.SEPARATOR)//.flatMap(elem => elem.split("[ |_]"))
  }

  def replaceSofiaAbbrev(str: String): String = {
    if (str == "manag") return "management"
    else if (str == "cogn") return "cognitive"
    str
  }

  // Filter out non-content words or not
  def selectWords(ws: Seq[String], contentOnly: Boolean): Seq[String] = {

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
    reader.wordToVec.makeCompositeVector(selectWords(words, contentOnly))
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
    var avg = 0.0f
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

    var avg: Double = 0.0
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
    var sum = 0.0f
    var i = 0
    while(i < v1.length) {
      sum += v1(i) * v2(i)
      i += 1
    }
    sum
  }

  def pairwiseScore(ce1: ConceptEmbedding, ce2: ConceptEmbedding, reader: EidosSystem): Float = {
    // Semantic similarity between leaf nodes (based on their examples)
    val examplesScore = dotProduct(ce1.embedding, ce2.embedding)
    // Semantic similarity of the parents (going up the hierarchy, more weight closer to leaves)
    val structureScore = weightedParentScore(ce1.namer.name, ce2.namer.name, reader)
    // Similarity between the indicator an the ontology concept ancestors
    val indicatorToAncestorScore = weightedNodeToParentScore(ce1.namer.name, ce2.namer.name, reader)

    0.8f * examplesScore + 0.1f * structureScore //+ 0.3 * indicatorToAncestorScore
  }

  def mostSimilarIndicators(concepts: Seq[ConceptEmbedding], indicators: Seq[ConceptEmbedding], n: Int = 10, reader: EidosSystem): Seq[(String, Seq[(String, Float)])] = {
    concepts.map(c => (c.namer.name, mostSimilar(c, indicators, n, reader)))
  }

  // n is to limit the number returned, 0 means return all
  def mostSimilar(concept: ConceptEmbedding, indicators: Seq[ConceptEmbedding], n: Int, reader: EidosSystem): Seq[(String, Float)] = {
    println(s"comparing $concept...")
    val comparisons = indicators.map(indicator => (indicator.namer.name, pairwiseScore(concept, indicator, reader)))//.filter(p => p._2 > 0.7)
    if (n > 0) {
      comparisons.sortBy(- _._2).take(n)
    } else {
      comparisons.sortBy(- _._2)
    }
  }

  // Mapping the UN_ONT to the indicator ontologies
  def mapIndicators(reader: EidosSystem, outputFile: String, topN: Int): Unit = {
    println(s"number of eidos ontologies - ${reader.loadableAttributes.ontologyGrounders.length}")
    val eidosConceptEmbeddings = reader.loadableAttributes.ontologyGrounders.head.conceptEmbeddings


    val eidosWDIConceptEmbeddings = reader.loadableAttributes.ontologyGrounders(1).conceptEmbeddings
    val eidosFAOConceptEmbeddings = reader.loadableAttributes.ontologyGrounders(2).conceptEmbeddings
    println(s"I think this will say WDI: ${reader.loadableAttributes.ontologyGrounders(1).name}")
    println(s"I think this will say FAO: ${reader.loadableAttributes.ontologyGrounders(2).name}")

    val un2fao = mostSimilarIndicators(eidosConceptEmbeddings, eidosFAOConceptEmbeddings, topN, reader).toMap
    un2fao.foreach(mapping => println(s"un: ${mapping._1} --> most similar FAO: ${mapping._2.mkString(",")}"))
    val un2wdi = mostSimilarIndicators(eidosConceptEmbeddings, eidosWDIConceptEmbeddings, topN, reader).toMap
    un2wdi.foreach(mapping => println(s"eidos: ${mapping._1} --> most similar WDI: ${mapping._2.mkString(",")}"))

    val pw = printWriterFromFile(outputFile)

    for (unConcept <- un2wdi.keys) {
      val wdiMappings = un2wdi(unConcept).map(p => (p._1, p._2, "WB"))
      val faoMappings = un2fao(unConcept).map(p => (p._1, p._2, "WB"))
      val sorted = (wdiMappings ++ faoMappings).sortBy(- _._2)
      for ((indicator, score, label) <- sorted) {
        pw.println(s"unConcept\t$unConcept\t$label\t$indicator\t$score")
      }
    }

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
    pw.close()
  }

  def mapOntologies(): Unit = {
    // Used if we are mapping to other teams' ontologies
    //  val sofiaFile = "/Users/bsharp/ech/onts/Sofia_Ontology.tbs"
    //  val bbnFile = "/Users/bsharp/Downloads/hume_ontology_examples.tsv"

    //  val sofiaConceptEmbeddings = loadOtherOntology(sofiaFile)
    //  val bbnConceptEmbeddings = loadOtherOntology(bbnFile)
    //  println("Finished loading other ontologies")


    // Make the table
    //  val topN = 0
    //  // current string, seq[other_string_orig!!, Double)
    //  val eidos2Sofia = mostSimilarIndicators(eidosConceptEmbeddings, sofiaConceptEmbeddings, topN)
    ////  eidos2Sofia.foreach(mapping => println(s"eidos: ${mapping._1} --> most similar sofia: ${mapping._2.mkString(",")}"))
    //  val eidos2BBN = mostSimilarIndicators(eidosConceptEmbeddings, bbnConceptEmbeddings, topN)
    ////  eidos2BBN.foreach(mapping => println(s"eidos: ${mapping._1} --> most similar BBN: ${mapping._2.mkString(",")}"))
    //  val sofia2BBN = mostSimilarIndicators(sofiaConceptEmbeddings, bbnConceptEmbeddings, topN)
    ////  sofia2BBN.foreach(mapping => println(s"sofia: ${mapping._1} --> most similar BBN: ${mapping._2.mkString(",")}"))
    //
    //  val pw = new PrintWriter(s"/Users/bsharp/ech/ontologyMappings_2018-07-30.tsv")
    //  for {
    //    (eidosConcept, sofiaMappings) <- eidos2Sofia
    //    (sofiaConcept, score) <- sofiaMappings
    //  } pw.println(s"eidos\t$eidosConcept\tsofia\t$sofiaConcept\t$score")
    //
    //  for {
    //    (eidosConcept, bbnMappings) <- eidos2BBN
    //    (bbnConcept, score) <- bbnMappings
    //  } pw.println(s"eidos\t$eidosConcept\tBBN\t$bbnConcept\t$score")
    //
    //  for {
    //    (sofiaConcept, bbnMappings) <- sofia2BBN
    //    (bbnConcept, score) <- bbnMappings
    //  } pw.println(s"sofia\t$sofiaConcept\tBBN\t$bbnConcept\t$score")
    //
    //  pw.close()
    ???
  }





}
