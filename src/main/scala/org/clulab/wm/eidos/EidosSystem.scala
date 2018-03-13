package org.clulab.wm.eidos

import java.util.Collection

import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.odin._
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.{Document, Processor, Sentence}
import org.clulab.sequences.LexiconNER
import org.clulab.utils.Configured
import org.clulab.wm.eidos.Aliases._
import org.clulab.wm.eidos.attachments.Score
import org.clulab.wm.eidos.entities.EidosEntityFinder
import org.clulab.wm.eidos.groundings.{AdjectiveGrounder, AdjectiveGrounding, EidosAdjectiveGrounder}
import org.clulab.wm.eidos.groundings.{OntologyGrounder, OntologyGrounding}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.DomainOntology
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sourcer

import com.typesafe.config.{Config, ConfigFactory}

case class AnnotatedDocument(var document: Document, var odinMentions: Seq[Mention], var eidosMentions: Seq[EidosMention])

/**
  * A system for text processing and information extraction
  */
class EidosSystem(val config: Config = ConfigFactory.load("eidos")) extends Configured with OntologyGrounder with AdjectiveGrounder {
  def this(x: Object) = this() // Dummy constructor crucial for Python integration
  val proc: Processor = new FastNLPProcessor() // TODO: Get from configuration file soon
  var debug = true // Allow external control with var
  var word2vec = getArgBoolean(getFullName("useW2V"), Some(false)) // Turn this on and off here

  override def getConf: Config = config  

  protected def getFullName(name: String) = EidosSystem.PREFIX + "." + name
  
  protected def getPath(name: String, defaultValue: String): String = {
    val path = getArgString(getFullName(name), Option(defaultValue))
    
    println(name + ": " + path)
    path
  }
  
  class LoadableAttributes(
      // These are the values which can be reloaded.  Query them for current assignments.
      val entityFinder: EidosEntityFinder, 
      val domainParamValues: Map[Param, Map[String, Double]],
      val adjectiveGrounder: AdjectiveGrounder,
      val actions: EidosActions,
      val engine: ExtractorEngine,
      val ner: LexiconNER,
      val stopWords: Set[String],
      val transparentWords: Set[String]
  )
  
  object LoadableAttributes {
    val   masterRulesPath: String = getPath(  "masterRulesPath", "/org/clulab/wm/eidos/grammars/master.yml")
    val  quantifierKBPath: String = getPath( "quantifierKBPath", "/org/clulab/wm/eidos/quantifierKB/gradable_adj_fullmodel.kb")
    val domainParamKBPath: String = getPath("domainParamKBPath", "/org/clulab/wm/eidos/quantifierKB/domain_parameters.kb")
    val    quantifierPath: String = getPath(   "quantifierPath",  "org/clulab/wm/eidos/lexicons/Quantifier.tsv")
    val   entityRulesPath: String = getPath(  "entityRulesPath", "/org/clulab/wm/eidos/grammars/entities/grammar/entities.yml")
    val    avoidRulesPath: String = getPath(   "avoidRulesPath", "/org/clulab/wm/eidos/grammars/avoidLocal.yml")
    val      taxonomyPath: String = getPath(     "taxonomyPath", "/org/clulab/wm/eidos/grammars/taxonomy.yml")
    val     stopwordsPath: String = getPath(    "stopWordsPath", "/org/clulab/wm/eidos/filtering/stops.txt")
    val   transparentPath: String = getPath(  "transparentPath", "/org/clulab/wm/eidos/filtering/transparent.txt")
    // This one is needed to construct some of the loadable attributes even though it isn't a path itself.
    val maxHops: Int = getArgInt(getFullName("maxHops"), Some(15))

    protected def loadDomainParams(domainParamKBFile: String): Map[Param, Map[String, Double]] = {
      FileUtils.getCommentedLinesFromSource(Sourcer.sourceFromResource(domainParamKBFile))
          .map { line => // line = [param]\t[variable]\t[value] => e.g. "rainfall  mean  30.5"
            val fields = line.split("\t")
            val param = fields(0)
            val var_values = fields.tail.map { var_value =>
              val tmp = var_value.split(":")
              val variable = tmp(0)
              val value = tmp(1).toDouble // Assuming the value of the variable is a double. TODO: Change this appropriately
              variable -> value
            }.toMap
            (param -> var_values)
          }.toMap
    }
    
    def apply(): LoadableAttributes = {
      // Reread these values from their files/resources each time based on paths in the config file.
      val masterRules = FileUtils.getTextFromResource(masterRulesPath)
      val actions = EidosActions(taxonomyPath)

      new LoadableAttributes(
          EidosEntityFinder(entityRulesPath, avoidRulesPath, maxHops = maxHops), 
          loadDomainParams(domainParamKBPath), 
          EidosAdjectiveGrounder(quantifierKBPath), 
          actions, 
          ExtractorEngine(masterRules, actions), // ODIN component 
          LexiconNER(Seq(quantifierPath), caseInsensitiveMatching = true), //TODO: keep Quantifier...
          FileUtils.getCommentedTextsFromResource(stopwordsPath).toSet,
          FileUtils.getCommentedTextsFromResource(transparentPath).toSet
      )
    }
  }
  
  var loadableAttributes = LoadableAttributes()

  // These public variables are accessed directly by clients and
  // the protected variables by local methods, neither of which
  // know they are loadable and which had better not keep copies.
  protected def entityFinder = loadableAttributes.entityFinder
  def domainParamValues = loadableAttributes.domainParamValues
  protected def actions = loadableAttributes.actions
  def engine = loadableAttributes.engine
  def ner = loadableAttributes.ner
  protected def stopWords = loadableAttributes.stopWords
  protected def transparentWords = loadableAttributes.transparentWords

  // These aren't intended to be (re)loadable.  This only happens once.
  val  wordToVecPath: String = getPath( "wordToVecPath", "/org/clulab/wm/eidos/sameas/vectors.txt")
  val domainOntoPath: String = getPath("domainOntoPath", "/org/clulab/wm/eidos/toy_ontology.yml")
  val topKNodeGroundings: Int = getArgInt(getFullName("topKNodeGroundings"), Some(10))

  val (w2v: Word2Vec, conceptEmbeddings: Map[String, Seq[Double]]) =
      initSameAsDataStructures(wordToVecPath, domainOntoPath)

  def reload() = loadableAttributes = LoadableAttributes()

  // Annotate the text using a Processor and then populate lexicon labels
  def annotate(text: String, keepText: Boolean = false): Document = {
    val doc = proc.annotate(text, keepText)
    doc.sentences.foreach(addLexiconNER)
    doc
  }

  protected def addLexiconNER(s: Sentence) = {
    for {
      (lexiconNERTag, i) <- ner.find(s).zipWithIndex
      if lexiconNERTag != EidosSystem.NER_OUTSIDE
    } s.entities.get(i) = lexiconNERTag
  }

  def extractFromText(text: String, keepText: Boolean = false, populateSameAs: Boolean = false): AnnotatedDocument = {
    val doc = annotate(text, keepText)
    val odinMentions = extractFrom(doc, populateSameAs = populateSameAs).toSeq
    val eidosMentions = EidosMention.asEidosMentions(odinMentions, this)
    
    new AnnotatedDocument(doc, odinMentions, eidosMentions)
  }
  
  // Be careful, because object may not be completely constructed.
  def groundOntology(mention: EidosMention): OntologyGrounding = {

    if (word2vec && mention.odinMention.matches("Entity")) { // TODO: Store this string somewhere
      val canonicalName = mention.canonicalName
      // Make vector for canonicalName
      val canonicalNameParts = canonicalName.split(" +")
      val nodeEmbedding = w2v.makeCompositeVector(canonicalNameParts)
      // Calc dot prods
      val similarities = conceptEmbeddings.toSeq.map(concept => (concept._1, Word2Vec.dotProduct(concept._2.toArray, nodeEmbedding)))
      // sort and return top k
      OntologyGrounding(similarities.sortBy(- _._2).slice(0, topKNodeGroundings))
    }
    else
      OntologyGrounding(Seq.empty)
  }

  def extractEventsFrom(doc: Document, state: State): Vector[Mention] = {
    val res = engine.extractFrom(doc, state).toVector
    val cleanMentions = actions.keepMostCompleteEvents(res, State(res)).toVector
//    val longest = actions.keepLongestMentions(cleanMentions, State(cleanMentions)).toVector
//   longest
    cleanMentions
  }

  def extractFrom(doc: Document, populateSameAs: Boolean = false): Vector[Mention] = {
    // get entities
    val entities = entityFinder.extractAndFilter(doc).toVector
    // filter entities which are entirely stop or transparent
//    println(s"In extractFrom() -- entities : ${entities.map(m => m.text).mkString(",\t")}")
    val filtered = filterStopTransparent(entities)
//    println(s"In extractFrom() -- filtered : ${filtered.map(m => m.text).mkString(",\t")}")
    val events = extractEventsFrom(doc, State(filtered)).distinct
//    if (!populateSameAs) return events
    //    println(s"In extractFrom() -- res : ${res.map(m => m.text).mkString(",\t")}")

    //    val sameAs = populateSameAsRelations(entities)
    //    events ++ sameAs
    events
  }

//    println(s"In extractFrom() -- entities : ${entities.map(m => m.text).mkString(",\t")}")
//    val unfilteredEntities = entityFinder.extract(doc).toVector
//    println(s"In extractFrom() -- entities_unfiltered : ${unfilteredEntities.map(m => m.text).mkString(",\t")}")


  def populateSameAsRelations(ms: Seq[Mention]): Seq[Mention] = {

    // Create an UndirectedRelation Mention to contain the sameAs grounding information
    def sameAs(a: Mention, b: Mention, score: Double): Mention = {
      // Build a Relation Mention (no trigger)
      new CrossSentenceMention(
        labels = Seq("SameAs"),
        anchor = a,
        neighbor = b,
        arguments = Seq(("node1", Seq(a)), ("node2", Seq(b))).toMap,
        document = a.document,  // todo: change?
        keep = true,
        foundBy = s"sameAs-${EidosSystem.SAME_AS_METHOD}",
        attachments = Set(Score(score)))
    }

    // n choose 2
    val sameAsRelations = for {
      (m1, i) <- ms.zipWithIndex
      m2 <- ms.slice(i+1, ms.length)
      score = calculateSameAs(m1, m2)
    } yield sameAs(m1, m2, score)

    sameAsRelations
  }

  // fixme: implement
  protected def calculateSameAs (m1: Mention, m2: Mention): Double = {
    val sanitisedM1 =  m1.text.split(" +").map( Word2Vec.sanitizeWord(_) )
    val sanitisedM2 =  m2.text.split(" +").map( Word2Vec.sanitizeWord(_) )
    val score = w2v.avgSimilarity(sanitisedM1, sanitisedM2)
    score
  }

  protected def initSameAsDataStructures (word2VecPath: String, ontologyPath: String): (Word2Vec, Map[String, Seq[Double]]) = {
    if (word2vec) {
      val ontology = DomainOntology(FileUtils.loadYamlFromResource(ontologyPath))
      val source = Sourcer.sourceFromResource(word2VecPath)
      try {
        val w2v = new Word2Vec(source, None)
        val conceptEmbeddings = ontology.iterateOntology(w2v)

        (w2v, conceptEmbeddings)
      }
      finally {
        source.close()
      }
    }
    else {
      val w2v = new Word2Vec(Map[String, Array[Double]]())
      val conceptEmbeddings = Map[String, Seq[Double]]()
      
      (w2v, conceptEmbeddings)
    }
  }

  def keepCAGRelavant(mentions: Seq[Mention]): Seq[Mention] = {
    mentions.filter(isCAGRelevant)
  }

  def isCAGRelevant(m:Mention): Boolean = {
    if (m.matches("Entity") && m.attachments.nonEmpty) {
      true
    }
    else if (EidosSystem.CAG_EDGES.contains(m.label)) {
      true
    }
    else {
      false
    }
  }


  /*
      Filtering
  */

  def containsStopword(stopword: String) =
      (stopWords ++ transparentWords).contains(stopword)

  def filterStopTransparent(mentions: Seq[Mention]): Seq[Mention] = {
    // remove mentions which are entirely stop/transparent words
    mentions.filter(hasContent)
  }

  def hasContent(m: Mention): Boolean = {
    val contentfulLemmas = m.lemmas.get.filterNot(lemma => (stopWords ++ transparentWords).contains(lemma))
    contentfulLemmas.nonEmpty
  }

  def groundAdjective(mention: Mention, quantifier: Quantifier): AdjectiveGrounding =
      loadableAttributes.adjectiveGrounder.groundAdjective(mention, quantifier)

  /*
     Debugging Methods
   */

  def debugPrint(str: String): Unit = if (debug) println(str)

  def debugMentions(mentions: Seq[Mention]): Unit = {
    if (debug) mentions.foreach(m => println(s" * ${m.text} [${m.label}, ${m.tokenInterval}]"))
  }
  
}

object EidosSystem {
  type Corpus = Seq[AnnotatedDocument]

  val PREFIX: String = "EidosSystem"
  
  val EXPAND_SUFFIX: String = "expandParams"
  val SPLIT_SUFFIX: String = "splitAtCC"
  val DEFAULT_DOMAIN_PARAM: String = "DEFAULT"
  val PARAM_MEAN: String = "mean"
  val PARAM_STDEV: String = "stdev"
  // Stateful Labels used by webapp
  val INC_LABEL_AFFIX = "-Inc"
  val DEC_LABEL_AFFIX = "-Dec"
  val QUANT_LABEL_AFFIX = "-Quant"
  val NER_OUTSIDE = "O"
  // Provenance info for sameAs scoring
  val SAME_AS_METHOD = "simple-w2v"
  // CAG filtering
  val CAG_EDGES = Set("Causal")
}
