package org.clulab.wm

import org.clulab.odin._

import org.clulab.processors.{Document, Processor}
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.sequences.LexiconNER

import org.clulab.wm.entities.OpenIEEntityFinder
import org.clulab.wm.wmutils.FileUtils.{findFilesFromResources, loadDomainParams, loadGradableAdjGroundingFile, readRules}
import Aliases._

/**
  * Handles text processing and information extraction for a domain.
  */
class OpenIESystem(
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
      val entityFinder: OpenIEEntityFinder, 
      val domainParamValues: Map[Param, Map[String, Double]],
      val grounder: Map[Quantifier, Map[String, Double]],
      val actions: OpenIEActions,
      val engine: ExtractorEngine,
      val ner: LexiconNER
  )
  
  object LoadableAttributes {
    def apply(): LoadableAttributes = {
      val rules = readRules(masterRulesPath)
      val actions = new OpenIEActions
     
      new LoadableAttributes(
          OpenIEEntityFinder(maxHops = 5), 
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

object OpenIESystem {

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
