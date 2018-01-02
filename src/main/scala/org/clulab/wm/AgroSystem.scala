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
  masterRulesPath: String = "/org/clulab/wm/grammars/master.yml",
  quantifierKBPath:String = "/org/clulab/wm/quantifierKB/gradable_adj_fullmodel.kb",
  domainParamKBPath:String = "/org/clulab/wm/quantifierKB/domain_parameters.kb",
  //agrovocLexiconsPath:String = "org/clulab/wm/agrovoc/lexicons",
  processor: Option[Processor] = None,
  debug: Boolean = true
) {

  // defaults to FastNLPProcessor if no processor is given
  val proc:Processor = if (processor.nonEmpty) processor.get else new FastNLPProcessor()

  // ODIN Rules Files
  val rules: String = readRules(masterRulesPath)


  val entityFinder: AgroEntityFinder = AgroEntityFinder(maxHops = 2)

  // Load the domain parameters (if param == 'all', apply the same values to all the parameters) //TODO: Change this appropriately
  val domainParamValues: Map[Param, Map[String, Double]] = loadDomainParams(domainParamKBPath)

  // Load the gradable adj grounding KB file
  val grounder: Map[Quantifier, Map[String, Double]] = loadGradableAdjGroundingFile(quantifierKBPath)

  // ODIN components
  val actions: AgroActions = new AgroActions
  val engine: ExtractorEngine = ExtractorEngine(rules, actions)

  // LexiconNER for labeling domain entities
  //TODO: agrovoc lexicons aren't in this project yet
  // todo: the order matters, we should be taking order into account
  //val agrovocLexicons = findFilesFromResources(agrovocLexiconsPath, "tsv")


  //TODO: keep Quantifier...
  val ner = LexiconNER(
    Seq("org/clulab/wm/lexicons/Quantifier.tsv"),
    caseInsensitiveMatching = true
  )

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

}


