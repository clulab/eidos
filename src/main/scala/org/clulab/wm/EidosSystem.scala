package org.clulab.wm

import org.clulab.odin._
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.{Document, Processor}
import org.clulab.sequences.LexiconNER
import org.clulab.wm.Aliases._
import org.clulab.wm.entities.EidosEntityFinder
import org.clulab.wm.wmutils.FileUtils.{loadDomainParams, loadGradableAdjGroundingFile, readRules}

/**
  * A system for text processing and information extraction
  */
class EidosSystem(
  // The first three are loaded as resources from URLs, thus the leading /
  // The last two are loaded as resources from files and have no leading /
      masterRulesPath: String = "/org/clulab/wm/grammars/master.yml",
     quantifierKBPath: String = "/org/clulab/wm/quantifierKB/gradable_adj_fullmodel.kb",
    domainParamKBPath: String = "/org/clulab/wm/quantifierKB/domain_parameters.kb",
       quantifierPath: String =  "org/clulab/wm/lexicons/Quantifier.tsv",
//agrovocLexiconsPath: String =  "org/clulab/wm/agrovoc/lexicons",
  processor: Option[Processor] = None,
  debug: Boolean = true
) {
  def this(x:Object) = this()

  // Defaults to FastNLPProcessor if no processor is given.  This is the expensive object
  // that should not be lost on a reload.  The "rules" that the processor follows are
  // not expected to change, or if they do, the processor would be restarted.
  val proc: Processor = if (processor.nonEmpty) processor.get else new FastNLPProcessor()

  class LoadableAttributes(
      val entityFinder: EidosEntityFinder, 
      val domainParamValues: Map[Param, Map[String, Double]],
      val grounder: Map[Quantifier, Map[String, Double]],
      val actions: EidosActions,
      val engine: ExtractorEngine,
      val ner: LexiconNER
  )
  
  object LoadableAttributes {
    def apply(): LoadableAttributes = {
      val rules = readRules(masterRulesPath)
      val actions = new EidosActions
     
      new LoadableAttributes(
          EidosEntityFinder(maxHops = 5), 
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
          ner = LexiconNER(Seq(quantifierPath), caseInsensitiveMatching = true) //TODO: keep Quantifier...
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

  def extractFrom(text: String, keepText: Boolean = false): Vector[Mention] = {
    val doc = annotate(text, keepText)
    extractFrom(doc)
  }
  
  def ground(mention: Mention, quantifier: Quantifier): EidosSystem.Grounding = {
    
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
      EidosSystem.Grounding(None, None, None)
    else {
      val intercept = modelRow.get(EidosSystem.INTERCEPT)
      val mu = modelRow.get(EidosSystem.MU_COEFF)
      val sigma = modelRow.get(EidosSystem.SIGMA_COEFF)
      
      EidosSystem.Grounding(intercept, mu, sigma)
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
    val unfilteredEntities = entityFinder.extract(doc).toVector
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

  case class Grounding(intercept: Option[Double], mu: Option[Double], sigma: Option[Double]) {
    def isGrounded = intercept != None && mu != None && sigma != None
  }
  
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
