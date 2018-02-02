package org.clulab.wm

import org.clulab.odin._

import org.clulab.processors.{Document, Processor}
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.sequences.LexiconNER

import org.clulab.wm.entities.AgroEntityFinder
import org.clulab.wm.wmutils.FileUtils.{findFilesFromResources, loadDomainParams, loadGradableAdjGroundingFile, readRules}
import Aliases._


/**
  * Handles text processing and information extraction for Agro domain.
  */
class AgroSystem(
  // In order to reload, these values must be saved.
  // The first three are loaded as resources from URLs, thus the leading /
  val   masterRulesPath:   String = "/org/clulab/wm/grammars/master.yml",
  val  quantifierKBPath:   String = "/org/clulab/wm/quantifierKB/gradable_adj_fullmodel.kb",
  val domainParamKBPath:   String = "/org/clulab/wm/quantifierKB/domain_parameters.kb",
  // The last two are loaded as resources from files and have no leading /
  val    quantifierPath:   String = "org/clulab/wm/lexicons/Quantifier.tsv",
//  val agrovocLexiconsPath: String = "org/clulab/wm/agrovoc/lexicons",
  processor: Option[Processor] = None,
  debug: Boolean = true
) {
  def this(x:Object) = this()

  // Defaults to FastNLPProcessor if no processor is given.  This is the expensive object
  // that should not be lost on a restart.  The "rules" that the processor follows are
  // not expected to change, or if they do, the processor would be restarted.
  val proc: Processor = if (processor.nonEmpty) processor.get else new FastNLPProcessor()
  
  protected var entityFinder: AgroEntityFinder = _
  var domainParamValues: Map[Param, Map[String, Double]] = _
  var grounder: Map[Quantifier, Map[String, Double]] = _
  protected var actions: AgroActions = _
  var engine: ExtractorEngine = _
  var ner: LexiconNER = _
  
  init()
  
  protected def init(): Unit = {
    // Both of these involve internal configuration files which may change.
    entityFinder = AgroEntityFinder(maxHops = 5)    
    // Load the domain parameters (if param == 'all', apply the same values to all the parameters) //TODO: Change this appropriately
    domainParamValues = loadDomainParams(domainParamKBPath)
    // Load the gradable adj grounding KB file
    grounder = loadGradableAdjGroundingFile(quantifierKBPath)
    
    val rules = readRules(masterRulesPath)
    actions = new AgroActions
    engine = ExtractorEngine(rules, actions) // ODIN component
    
    // LexiconNER for labeling domain entities
    //TODO: agrovoc lexicons aren't in this project yet
    // todo: the order matters, we should be taking order into account
    //val agrovocLexicons = findFilesFromResources(agrovocLexiconsPath, "tsv")
    ner = LexiconNER(Seq(quantifierPath), caseInsensitiveMatching = true) //TODO: keep Quantifier...
  }
  
  def reload() = init
  
  def annotate(text: String): Document = {
    val doc = proc.annotate(text)
    doc.sentences.foreach(s => s.entities = Some(ner.find(s)))
    doc
  }

  def extractFrom(text: String): Vector[Mention] = {
    val doc = annotate(text)
    extractFrom(doc)
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
    // get events
    val res = extractEventsFrom(doc, State(entities)).distinct
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

object AgroSystem {

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
