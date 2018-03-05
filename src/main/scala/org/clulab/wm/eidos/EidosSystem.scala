package org.clulab.wm.eidos

import org.clulab.odin._
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.{Document, Processor}
import org.clulab.sequences.LexiconNER
import org.clulab.utils.Configured
import org.clulab.wm.eidos.Aliases._
import org.clulab.wm.eidos.entities.EidosEntityFinder
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.FileUtils.{loadDomainParams, loadGradableAdjGroundingFile, readRules}
import com.typesafe.config.{Config, ConfigFactory}

trait EntityGrounder {
  def ground(mention: Mention, quantifier: Quantifier): Grounding
}

case class Grounding(intercept: Option[Double], mu: Option[Double], sigma: Option[Double]) {
  def isGrounded = intercept != None && mu != None && sigma != None
}

case class AnnotatedDocument(var document: Document, var odinMentions: Seq[Mention], var eidosMentions: Seq[EidosMention])

/**
  * A system for text processing and information extraction
  */
class EidosSystem(val config: Config = ConfigFactory.load("eidos")) extends EntityGrounder with Configured {
  def this(x: Object) = this() // Dummy constructor crucial for Python integration
  val proc: Processor = new FastNLPProcessor() // TODO: Get from configuration file soon
  var debug = true // Allow external control with var

  override def getConf: Config = config  
  
  class LoadableAttributes(
      val entityFinder: EidosEntityFinder, 
      val domainParamValues: Map[Param, Map[String, Double]],
      val grounder: Map[Quantifier, Map[String, Double]],
      val actions: EidosActions,
      val engine: ExtractorEngine,
      val ner: LexiconNER
  )
  
  object LoadableAttributes {
    
    protected def getFullName(name: String) = EidosSystem.PREFIX + "." + name
    
    protected def getPath(name: String, defaultValue: String): String =
        getArgString(getFullName(name), Option(defaultValue))
      
    val   masterRulesPath: String = getPath(  "masterRulesPath", "/org/clulab/wm/eidos/grammars/master.yml")
    val  quantifierKBPath: String = getPath( "quantifierKBPath", "/org/clulab/wm/eidos/quantifierKB/gradable_adj_fullmodel.kb")
    val domainParamKBPath: String = getPath("domainParamKBPath", "/org/clulab/wm/eidos/quantifierKB/domain_parameters.kb")
    val    quantifierPath: String = getPath(   "quantifierPath",  "org/clulab/wm/eidos/lexicons/Quantifier.tsv")
    val   entityRulesPath: String = getPath(  "entityRulesPath", "/org/clulab/wm/eidos/grammars/entities/grammar/entities.yml")
    val    avoidRulesPath: String = getPath(   "avoidRulesPath", "/org/clulab/wm/eidos/grammars/avoidLocal.yml")
    val      taxonomyPath: String = getPath(     "taxonomyPath",  "org/clulab/wm/eidos/grammars/taxonomy.yml")
    
    val maxHops: Int = getArgInt(getFullName("maxHops"), Option(15))
      
    // Get these instead from the configuration
    def apply(): LoadableAttributes = {
      val rules = readRules(masterRulesPath)
      val actions = EidosActions(taxonomyPath)
     
      new LoadableAttributes(
          EidosEntityFinder(entityRulesPath, avoidRulesPath, maxHops = maxHops), 
          // Load the domain parameters (if param == 'all', apply the same values to all the parameters) //TODO: Change this appropriately
          loadDomainParams(domainParamKBPath), 
          // Load the gradable adj grounding KB file
          loadGradableAdjGroundingFile(quantifierKBPath), 
          actions, 
          ExtractorEngine(rules, actions), // ODIN component 
          // LexiconNER for labeling domain entities
          //TODO: agrovoc lexicons aren't in this project yet
          // todo: the order matters, we should be taking order into account
          //val agrovocLexicons = findFilesFromResources(agrovocLexiconsPath, "tsv")
          LexiconNER(Seq(quantifierPath), caseInsensitiveMatching = true) //TODO: keep Quantifier...
      )
    }
  }
  
  var loadableAttributes = LoadableAttributes()
  
  // These public variables are accessed directly by clients and
  // the protected variables by local methods, neither of which
  // know they are loadable and which had better not keep copies.
  protected def entityFinder = loadableAttributes.entityFinder
  def domainParamValues = loadableAttributes.domainParamValues
  def grounder = loadableAttributes.grounder
  protected def actions = loadableAttributes.actions
  def engine = loadableAttributes.engine
  def ner = loadableAttributes.ner
  
  def reload() = loadableAttributes = LoadableAttributes()
  
  def annotate(text: String, keepText: Boolean = false): Document = {
    val doc = proc.annotate(text, keepText)
    doc.sentences.foreach(s => s.entities = Some(ner.find(s)))
    doc
  }

  def extractFrom(text: String, keepText: Boolean = false): AnnotatedDocument = {
    val doc = annotate(text, keepText)
    val odinMentions = extractFrom(doc)
    val eidosMentions = EidosMention.asEidosMentions(odinMentions)
    
    new AnnotatedDocument(doc, odinMentions, eidosMentions)
  }
  
  def ground(mention: Mention, quantifier: Quantifier): Grounding = {
    
    def stemIfAdverb(word: String) = {
      if (word.endsWith("ly"))
        if (word.endsWith("ily"))
          word.slice(0, word.length - 3) ++ "y"
        else
          word.slice(0, word.length - 2)
      else
        word
    }
    
    val pseudoStemmed = stemIfAdverb(quantifier)
    val modelRow = grounder.getOrElse(pseudoStemmed, Map.empty)
    
    if (modelRow.isEmpty)
      Grounding(None, None, None)
    else {
      val intercept = modelRow.get(EidosSystem.INTERCEPT)
      val mu = modelRow.get(EidosSystem.MU_COEFF)
      val sigma = modelRow.get(EidosSystem.SIGMA_COEFF)
      
      Grounding(intercept, mu, sigma)
    }
  }

//TODO: starting here :)

  def extractEventsFrom(doc: Document, state: State): Vector[Mention] = {
    val res = engine.extractFrom(doc, state).toVector
    val cleanMentions = actions.keepMostCompleteEvents(res, State(res)).toVector
//    val longest = actions.keepLongestMentions(cleanMentions, State(cleanMentions)).toVector
//   longest
    cleanMentions
  }

  def extractFrom(doc: Document): Vector[Mention] = {
    // get entities
    val entities = entityFinder.extractAndFilter(doc).toVector
//    println(s"In extractFrom() -- entities : ${entities.map(m => m.text).mkString(",\t")}")
//    val unfilteredEntities = entityFinder.extract(doc).toVector
//    println(s"In extractFrom() -- entities_unfiltered : ${unfilteredEntities.map(m => m.text).mkString(",\t")}")
    // get events
    val res = extractEventsFrom(doc, State(entities)).distinct
//    println(s"In extractFrom() -- res : ${res.map(m => m.text).mkString(",\t")}")
    res
  }


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
  val MU_COEFF: String = "mu_coeff"
  val SIGMA_COEFF: String = "sigma_coeff"
  val INTERCEPT: String = "intercept"
  val PARAM_MEAN: String = "mean"
  val PARAM_STDEV: String = "stdev"
  // Stateful Labels used by webapp
  val INC_LABEL_AFFIX = "-Inc"
  val DEC_LABEL_AFFIX = "-Dec"
  val QUANT_LABEL_AFFIX = "-Quant"
}
