package org.clulab.wm.eidos

import com.typesafe.config.{Config, ConfigFactory}

import org.clulab.odin._
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.{Document, Processor, Sentence}
import org.clulab.sequences.LexiconNER
import org.clulab.utils.Configured
import org.clulab.wm.eidos.Aliases._
import org.clulab.wm.eidos.attachments.Score
import org.clulab.wm.eidos.entities.EidosEntityFinder
import org.clulab.wm.eidos.groundings.{AdjectiveGrounder, AdjectiveGrounding, EidosAdjectiveGrounder}
import org.clulab.wm.eidos.groundings.{OntologyGrounder, OntologyGrounding, EidosOntologyGrounder}
import org.clulab.wm.eidos.groundings.EidosWordToVec
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.DomainParams
import org.clulab.wm.eidos.utils.FileUtils

import org.slf4j.LoggerFactory

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

    EidosSystem.logger.info(name + ": " + path)
    path
  }

  class LoadableAttributes(
                            // These are the values which can be reloaded.  Query them for current assignments.
                            val entityFinder: EidosEntityFinder,
                            val domainParams: DomainParams,
                            val adjectiveGrounder: AdjectiveGrounder,
                            val actions: EidosActions,
                            val engine: ExtractorEngine,
                            val ner: LexiconNER,
                            val ontologyGrounder: EidosOntologyGrounder
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

    def apply(): LoadableAttributes = {
      // Reread these values from their files/resources each time based on paths in the config file.
      val masterRules = FileUtils.getTextFromResource(masterRulesPath)
      val actions = EidosActions(taxonomyPath)

      new LoadableAttributes(
        EidosEntityFinder(entityRulesPath, avoidRulesPath, maxHops = maxHops),
        DomainParams(domainParamKBPath),
        EidosAdjectiveGrounder(quantifierKBPath),
        actions,
        ExtractorEngine(masterRules, actions, actions.mergeAttachments), // ODIN component
        LexiconNER(Seq(quantifierPath), caseInsensitiveMatching = true), //TODO: keep Quantifier...
        EidosOntologyGrounder(stopwordsPath, transparentPath)
      )
    }
  }

  var loadableAttributes = LoadableAttributes()

  // These public variables are accessed directly by clients which
  // don't know they are loadable and which had better not keep copies.
  def domainParams = loadableAttributes.domainParams
  def engine = loadableAttributes.engine
  def ner = loadableAttributes.ner

  // This isn't intended to be (re)loadable.  This only happens once.
  protected val wordToVec = EidosWordToVec(
    word2vec,
    getPath(     "wordToVecPath", "/org/clulab/wm/eidos/w2v/vectors.txt"),
    getPath("domainOntologyPath", "/org/clulab/wm/eidos/ontology.yml"),
    getArgInt(getFullName("topKNodeGroundings"), Some(10))
  )

  def reload() = loadableAttributes = LoadableAttributes()

  /*
      Extraction Methods
  */

  // MAIN PIPELINE METHOD
  def extractFromText(text: String, keepText: Boolean = false): AnnotatedDocument = {
    // Step 1: Annotate the text
    val doc = annotate(text, keepText)
    // Step 2: Get the odin events
    val odinMentions = extractFrom(doc)
    // Step 3: Filter out the entities that are never used in an event we care about
    val cagRelevant = keepCAGRelevant(odinMentions)
    // Step 4: Convert the odin Mentions to EidosMentions, grounding to the taxonomy happens here
    val eidosMentions = EidosMention.asEidosMentions(cagRelevant, this)

    new AnnotatedDocument(doc, cagRelevant, eidosMentions)
  }

  // Method that gets Mentions from a Document
  def extractFrom(doc: Document): Vector[Mention] = {
    // 1. Get entities
    val entities = loadableAttributes.entityFinder.extractAndFilter(doc).toVector
    //println(s"In extractFrom() -- entities : \n\t${entities.map(m => m.text).sorted.mkString("\n\t")}")

    // 2. Filter entities which are entirely stop or transparent
    val filtered = loadableAttributes.ontologyGrounder.filterStopTransparent(entities)
    //println(s"\nAfter filterStopTransparent() -- entities : \n\t${filtered.map(m => m.text).sorted.mkString("\n\t")}")

    // 3. Extract events that involve these entiies
    val events = extractEventsFrom(doc, State(filtered)).distinct
    //println(s"In extractFrom() -- res : ${res.map(m => m.text).mkString(",\t")}")

    events
  }

  // Helper method, runs the event grammars.  The state needs to be populated with the previously found entities.
  def extractEventsFrom(doc: Document, state: State): Vector[Mention] = {
    val res = engine.extractFrom(doc, state).toVector
    loadableAttributes.actions.keepMostCompleteEvents(res, State(res)).toVector
  }

  /*
      Pre- and Post-Processing Methods
  */

  // Annotate the text using a Processor and then populate lexicon labels
  def annotate(text: String, keepText: Boolean = false): Document = {
    def addLexiconNER(s: Sentence) = {
      for {
        (lexiconNERTag, i) <- ner.find(s).zipWithIndex
        if lexiconNERTag != EidosSystem.NER_OUTSIDE
      } s.entities.get(i) = lexiconNERTag
    }

    val doc = proc.annotate(text, keepText)
    doc.sentences.foreach(addLexiconNER)
    doc
  }

  // Filter mentions to only those which are of the type desired for downstream use.
  def keepCAGRelevant(mentions: Seq[Mention]): Seq[Mention] = {
    val cagEdgeMentions = mentions.filter(m => EidosSystem.CAG_EDGES.contains(m.label))
    mentions.filter(m => isCAGRelevant(m, cagEdgeMentions))
  }

  def isCAGRelevant(m:Mention, cagEdgeMentions: Seq[Mention]): Boolean =
      (m.matches("Entity") && m.attachments.nonEmpty) ||
          cagEdgeMentions.exists(cm => cm.arguments.values.flatten.toSeq.contains(m)) ||
          cagEdgeMentions.contains(m)
  
  /*
      Grounding Methods
  */

  // Ground a mention to the ontology:
  //   Returns a Seq[(String, Double)] of top k soft groundings to an ontology.
  def groundOntology(mention: EidosMention): OntologyGrounding =
    loadableAttributes.ontologyGrounder.groundOntology(mention, wordToVec)

  // Check to see if a word is a stopword or not
  def containsStopword(stopword: String) =
    loadableAttributes.ontologyGrounder.containsStopword(stopword)

  // Ground a gradable adjective to the LREC2018 adjectives model
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

  val logger = LoggerFactory.getLogger(this.getClass())

  val PREFIX: String = "EidosSystem"

  val EXPAND_SUFFIX: String = "expandParams"
  val SPLIT_SUFFIX: String = "splitAtCC"
  // Stateful Labels used by webapp
  val INC_LABEL_AFFIX = "-Inc"
  val DEC_LABEL_AFFIX = "-Dec"
  val QUANT_LABEL_AFFIX = "-Quant"
  val NER_OUTSIDE = "O"
  // Provenance info for sameAs scoring
  val SAME_AS_METHOD = "simple-w2v"

  // CAG filtering
  val CAG_EDGES = Set("Causal", "Correlation")

}