package org.clulab.wm.eidos.apps

import java.io.PrintWriter

import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.{ConceptEmbedding, OntologyNode}

object ParsePapers extends App {

  val reader = new EidosSystem()
  val w2v = reader.wordToVec


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
    } yield new ConceptEmbedding(path.mkString(OntologyNode.SEPARATOR), embedding)

    //source.close()

    ces
  }

  // todo: do I need to split on underscore -- you betcha!
  def getParents(path: String): Seq[String] = {
    path.split(OntologyNode.SEPARATOR)//.flatMap(elem => elem.split("[ |_]"))
  }

  def replaceSofiaAbbrev(str: String): String = {
    if (str == "manag") return "management"
    else if (str == "cogn") return "cognitive"
    str
  }

  def mkMWEmbedding(s: String): Array[Double] = {
    val words = s.split("[ |_]").map(Word2Vec.sanitizeWord(_)).map(replaceSofiaAbbrev)
    reader.wordToVec.makeCompositeVector(words)
  }

  def mweStringSimilarity(a: String, b: String): Double = {
    dotProduct(mkMWEmbedding(a), mkMWEmbedding(b))
  }

  def weightedParentScore(path1: String, path2: String): Double = {
    // like entity/natural_resource/water ==> Seq(entity, natural_resource, water)
    // reverse: Seq(water, natural_resource, water)
    val parents1 = getParents(path1).reverse
    val parents2 = getParents(path2).reverse
    val k = math.min(parents1.length, parents2.length)
    var avg: Double = 0.0
    for (i <- 0 until k) {
      val score = mweStringSimilarity(parents1(i), parents2(i))
      avg += score / (i + 1)
    }
    avg
  }

  def dotProduct(v1:Array[Double], v2:Array[Double]):Double = {
    assert(v1.length == v2.length) //should we always assume that v2 is longer? perhaps set shorter to length of longer...
    var sum = 0.0
    var i = 0
    while(i < v1.length) {
      sum += v1(i) * v2(i)
      i += 1
    }
    sum
  }

  def pairwiseScore(ce1: ConceptEmbedding, ce2: ConceptEmbedding): Double = {
    // Semantic similarity between leaf nodes (based on their examples)
    val examplesScore = dotProduct(ce1.embedding, ce2.embedding)
    // Semantic similarity of the parents (going up the hierarchy, more weight closer to leaves)
    val structureScore = weightedParentScore(ce1.concept, ce2.concept)

    0.7 * examplesScore + 0.3 * structureScore
  }

  def mostSimilarIndicators(concepts: Seq[ConceptEmbedding], indicators: Seq[ConceptEmbedding], n: Int = 10): Seq[(String, Seq[(String, Double)])] = {
    concepts.map(c => (c.concept, mostSimilar(c, indicators, n)))
  }

  def mostSimilar(concept: ConceptEmbedding, indicators: Seq[ConceptEmbedding], n: Int): Seq[(String, Double)] = {
    val comparisons = indicators.map(indicator => (indicator.concept, pairwiseScore(concept, indicator)))
    if (n > 0) {
      comparisons.sortBy(- _._2).take(n)
    } else {
      comparisons.sortBy(- _._2)
    }
  }

  println("Getting started!")

  val sofiaFile = "/Users/bsharp/ech/onts/Sofia_Ontology.tbs"
//  val bbnFile = "/Users/bsharp/ech/onts/bbn_ontology_examples.tsv"
  val bbnFile = "/Users/bsharp/Downloads/hume_ontology_examples.tsv"


  println(s"number of eidos ontologies - ${reader.loadableAttributes.ontologyGrounders.length}")
  val eidosConceptEmbeddings = reader.loadableAttributes.ontologyGrounders.head.conceptEmbeddings
  val sofiaConceptEmbeddings = loadOtherOntology(sofiaFile)
  val bbnConceptEmbeddings = loadOtherOntology(bbnFile)
  println("Finished loading other ontologies")


  // Make the table
  val topN = 0
  // current string, seq[other_string_orig!!, Double)
  val eidos2Sofia = mostSimilarIndicators(eidosConceptEmbeddings, sofiaConceptEmbeddings, topN)
//  eidos2Sofia.foreach(mapping => println(s"eidos: ${mapping._1} --> most similar sofia: ${mapping._2.mkString(",")}"))
  val eidos2BBN = mostSimilarIndicators(eidosConceptEmbeddings, bbnConceptEmbeddings, topN)
//  eidos2BBN.foreach(mapping => println(s"eidos: ${mapping._1} --> most similar BBN: ${mapping._2.mkString(",")}"))
  val sofia2BBN = mostSimilarIndicators(sofiaConceptEmbeddings, bbnConceptEmbeddings, topN)
//  sofia2BBN.foreach(mapping => println(s"sofia: ${mapping._1} --> most similar BBN: ${mapping._2.mkString(",")}"))

  val pw = new PrintWriter(s"/Users/bsharp/ech/ontologyMappings_2018-07-30.tsv")
  for {
    (eidosConcept, sofiaMappings) <- eidos2Sofia
    (sofiaConcept, score) <- sofiaMappings
  } pw.println(s"eidos\t$eidosConcept\tsofia\t$sofiaConcept\t$score")

  for {
    (eidosConcept, bbnMappings) <- eidos2BBN
    (bbnConcept, score) <- bbnMappings
  } pw.println(s"eidos\t$eidosConcept\tBBN\t$bbnConcept\t$score")

  for {
    (sofiaConcept, bbnMappings) <- sofia2BBN
    (bbnConcept, score) <- bbnMappings
  } pw.println(s"sofia\t$sofiaConcept\tBBN\t$bbnConcept\t$score")

  pw.close()

//  val topN = 10
//  val eidosWDIConceptEmbeddings = reader.loadableAttributes.ontologyGrounders(1).conceptEmbeddings
//  val eidosFAOConceptEmbeddings = reader.loadableAttributes.ontologyGrounders(2).conceptEmbeddings
//  println(s"I think this will say WDI: ${reader.loadableAttributes.ontologyGrounders(1).name}")
//  println(s"I think this will say FAO: ${reader.loadableAttributes.ontologyGrounders(2).name}")
//
//  val un2fao = mostSimilarIndicators(eidosConceptEmbeddings, eidosFAOConceptEmbeddings, topN)
//  un2fao.foreach(mapping => println(s"un: ${mapping._1} --> most similar FAO: ${mapping._2.mkString(",")}"))
//  val un2wdi = mostSimilarIndicators(eidosConceptEmbeddings, eidosWDIConceptEmbeddings, topN)
//  un2wdi.foreach(mapping => println(s"eidos: ${mapping._1} --> most similar WDI: ${mapping._2.mkString(",")}"))
//
//  val pw = new PrintWriter("/Users/bsharp/ech/indicatorMappings.tsv")
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
//    pw.close()


  //
//  val mostSimilar = mostSimilarIndicators(eidosConceptEmbeddings, eidosFAOConceptEmbeddings ++ eidosWDIConceptEmbeddings)





}
