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
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.groundings.Aliases.Groundings
import org.clulab.wm.eidos.groundings.EidosOntologyGrounder.{UN_NAMESPACE, WDI_NAMESPACE, FAO_NAMESPACE}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.{DomainParams, FileUtils, StopwordManager, StopwordManaging}
import org.slf4j.LoggerFactory

case class AnnotatedDocument(var document: Document, var odinMentions: Seq[Mention], var eidosMentions: Seq[EidosMention])

/**
  * A system for text processing and information extraction
  */
class EidosSystem(val config: Config = ConfigFactory.load("eidos")) extends Configured with StopwordManaging with MultiOntologyGrounder with AdjectiveGrounder {
  def this(x: Object) = this() // Dummy constructor crucial for Python integration
  val proc: Processor = new FastNLPProcessor() // TODO: Get from configuration file soon
  var debug = true // Allow external control with var

  override def getConf: Config = config

  var word2vec = getArgBoolean(getFullName("useW2V"), Some(false)) // Turn this on and off here
  protected val wordToVec = EidosWordToVec(
    word2vec,
    getPath("wordToVecPath", "/org/clulab/wm/eidos/w2v/vectors.txt"),
    getArgInt(getFullName("topKNodeGroundings"), Some(10))
  )

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
    val stopwordManager: StopwordManager,
    val ontologyGrounders: Seq[EidosOntologyGrounder]
  )

  object LoadableAttributes {
    val    masterRulesPath: String = getPath(   "masterRulesPath", "/org/clulab/wm/eidos/grammars/master.yml")
    val   quantifierKBPath: String = getPath(  "quantifierKBPath", "/org/clulab/wm/eidos/quantifierKB/gradable_adj_fullmodel.kb")
    val  domainParamKBPath: String = getPath("domainParamKBPath", "/org/clulab/wm/eidos/quantifierKB/domain_parameters.kb")
    val     quantifierPath: String = getPath(    "quantifierPath",  "org/clulab/wm/eidos/lexicons/Quantifier.tsv")
    val    entityRulesPath: String = getPath(   "entityRulesPath", "/org/clulab/wm/eidos/grammars/entities/grammar/entities.yml")
    val     avoidRulesPath: String = getPath(    "avoidRulesPath", "/org/clulab/wm/eidos/grammars/avoidLocal.yml")
    val       taxonomyPath: String = getPath(      "taxonomyPath", "/org/clulab/wm/eidos/grammars/taxonomy.yml")
    val      stopwordsPath: String = getPath(     "stopWordsPath", "/org/clulab/wm/eidos/filtering/stops.txt")
    val    transparentPath: String = getPath(   "transparentPath", "/org/clulab/wm/eidos/filtering/transparent.txt")

    val domainOntologyPath: String = getPath("domainOntologyPath", "/org/clulab/wm/eidos/ontology.yml")
    val     unOntologyPath: String = getPath(    "unOntologyPath", "/org/clulab/wm/eidos/un_ontology.yml")
    val    wdiOntologyPath: String = getPath(   "wdiOntologyPath", "/org/clulab/wm/eidos/wdi_ontology.yml")
    val    faoOntologyPath: String = getPath(       "faoOntology", "/org/clulab/wm/eidos/fao_variable_ontology.yml")

    // These are needed to construct some of the loadable attributes even though it isn't a path itself.
    val ontologies: Seq[String] = getArgStrings(getFullName("ontologies"), Some(Seq.empty))
    val maxHops: Int = getArgInt(getFullName("maxHops"), Some(15))

    protected def ontologyGrounders: Seq[EidosOntologyGrounder] =
        if (!word2vec)
          Seq.empty
        else if (ontologies.isEmpty)
          Seq(new DomainOntologyGrounder("domain", domainOntologyPath, wordToVec))
        else {
          for (ontology <- ontologies)
          yield ontology match {
            case name @ UN_NAMESPACE  => new  UNOntologyGrounder(name,  unOntologyPath, wordToVec)
            case name @ WDI_NAMESPACE => new WDIOntologyGrounder(name, wdiOntologyPath, wordToVec)
            case name @ FAO_NAMESPACE => new FAOOntologyGrounder(name, faoOntologyPath, wordToVec)
            case name @ _ => throw new IllegalArgumentException("Ontology " + name + " is not recognized.")
          }
        }

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
        StopwordManager(stopwordsPath, transparentPath),
        ontologyGrounders
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

  // MAIN PIPELINE METHOD
  def extractFromText(text: String, keepText: Boolean = false): AnnotatedDocument = {
    val doc = annotate(text, keepText)
    val odinMentions = extractFrom(doc)
    println(s"\nodinMentions() -- entities : \n\t${odinMentions.map(m => m.text).sorted.mkString("\n\t")}")
    val cagRelevant = keepCAGRelevant(odinMentions)
    val eidosMentions = EidosMention.asEidosMentions(cagRelevant, loadableAttributes.stopwordManager, this)

    new AnnotatedDocument(doc, cagRelevant, eidosMentions)
  }

  def extractEventsFrom(doc: Document, state: State): Vector[Mention] = {
    val res = engine.extractFrom(doc, state).toVector
    loadableAttributes.actions.keepMostCompleteEvents(res, State(res)).toVector
  }

  def extractFrom(doc: Document): Vector[Mention] = {
    // get entities
    val entities: Seq[Mention] = loadableAttributes.entityFinder.extractAndFilter(doc).toVector

    // filter entities which are entirely stop or transparent
    //println(s"In extractFrom() -- entities : \n\t${entities.map(m => m.text).sorted.mkString("\n\t")}")
    // Becky says not to filter yet
    //val filtered = loadableAttributes.ontologyGrounder.filterStopTransparent(entities)
    val filtered = entities
    //println(s"\nAfter filterStopTransparent() -- entities : \n\t${filtered.map(m => m.text).sorted.mkString("\n\t")}")

    val events = extractEventsFrom(doc, State(filtered)).distinct
    // Note -- in main pipeline we filter to only CAG relevant after this method.  Since the filtering happens at the
    // next stage, currently all mentions make it to the webapp, even ones that we filter out for the CAG exports
    //val cagRelevant = keepCAGRelevant(events)

    events
    //cagRelevant.toVector
  }


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
      score = wordToVec.calculateSimilarity(m1, m2)
    } yield sameAs(m1, m2, score)

    sameAsRelations
  }

  // Old version
  def oldKeepCAGRelevant(mentions: Seq[Mention]): Seq[Mention] = {
    // 1) These will be "Causal" and "Correlation" which fall under "Event"
    val cagEdgeMentions = mentions.filter(m => EidosSystem.CAG_EDGES.contains(m.label))
    // 2) and these will be "Entity", without overlap from above.
    val entityMentions = mentions.filter(m => m.matches("Entity") && m.attachments.nonEmpty)
    // 3) These last ones may overlap with the above or include mentions not in the original list.
    val argumentMentions: Seq[Mention] = cagEdgeMentions.flatMap(_.arguments.values.flatten)
    // Put them all together.
    val goodMentions = cagEdgeMentions ++ entityMentions ++ argumentMentions
    // To preserve order, avoid repeats, and not allow anything new in the list, filter the original.
    val relevantMentions = mentions.filter(m => goodMentions.exists(m.eq))

    relevantMentions
  }

  // New version
  def keepCAGRelevant(mentions: Seq[Mention]): Seq[Mention] = {

    // 1) These will be "Causal" and "Correlation" which fall under "Event" if they have content
    val cagEdgeMentions = mentions.filter(m => releventEdge(m))

    // Should these be included as well?

    // 3) These last ones may overlap with the above or include mentions not in the original list.
    val argumentMentions: Seq[Mention] = cagEdgeMentions.flatMap(_.arguments.values.flatten)
    // Put them all together.
    val goodMentions = cagEdgeMentions ++ argumentMentions
    // To preserve order, avoid repeats, and not allow anything new in the list, filter the original.
    val relevantMentions = mentions.filter(m => goodMentions.exists(m.eq))

//    relevantMentions
    goodMentions
  }

  def releventEdge(m: Mention): Boolean = {
    m match {
      case tb: TextBoundMention => EidosSystem.CAG_EDGES.contains(tb.label)
      case rm: RelationMention => EidosSystem.CAG_EDGES.contains(rm.label)
      case em: EventMention => EidosSystem.CAG_EDGES.contains(em.label) && argumentsHaveContent(em)
      case _ => throw new UnsupportedClassVersionError()
    }
  }

  def argumentsHaveContent(mention: EventMention): Boolean = {
    val causes: Seq[Mention] = mention.arguments.getOrElse("cause", Seq.empty)
    val effects: Seq[Mention] = mention.arguments.getOrElse("effect", Seq.empty)

    if (causes.nonEmpty && effects.nonEmpty) // If it's something interesting,
    // then both causes and effects should have some content
      causes.exists(loadableAttributes.stopwordManager.hasContent(_)) && effects.exists(loadableAttributes.stopwordManager.hasContent(_))
    else
      true
  }

  /*
      Grounding
  */

  def containsStopword(stopword: String) =
    loadableAttributes.stopwordManager.containsStopword(stopword)

  def groundOntology(mention: EidosMention): Groundings =
      if (!word2vec)
        Map.empty
      else
        loadableAttributes.ontologyGrounders.map (ontologyGrounder =>
          (ontologyGrounder.name, ontologyGrounder.groundOntology(mention))).toMap

  def groundAdjective(mention: Mention, quantifier: Quantifier): AdjectiveGrounding =
    loadableAttributes.adjectiveGrounder.groundAdjective(mention, quantifier)

  /*
      Wrapper for using w2v on some strings
   */
  def stringSimilarity(s1: String, s2: String): Double = wordToVec.stringSimilarity(s1, s2)

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