package org.clulab.wm

import org.clulab.odin._

import org.clulab.processors.{Document, Processor}
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.sequences.LexiconNER

import scala.collection.mutable
import org.clulab.wm.entities.AgroEntityFinder
import org.clulab.wm.wmutils.FileUtils.{findFilesFromResources, loadDomainParams, loadGradableAdjGroundingFile, readRules}





/**
  * Handles text processing and information extraction for Agro domain.
  */
class AgroSystem(
  masterRulesPath: String = "/org/clulab/wm/grammars/master.yml",
  quantifierKBPath:String = "/org/clulab/wm/quantifierKB/gradable_adj_fullmodel.kb",
  //domainParamKBPath:String = "/org/clulab/wm/quantifierKB/domain_parameters.kb",
  //agrovocLexiconsPath:String = "org/clulab/wm/agrovoc/lexicons",
  processor: Option[Processor] = None,
  debug: Boolean = false
) {

  // defaults to FastNLPProcessor if no processor is given
  val proc = if (processor.nonEmpty) processor.get else new FastNLPProcessor()

  // ODIN Rules Files
  val rules: String = readRules(masterRulesPath)


  val entityFinder = AgroEntityFinder(maxHops = 2)

  // Load the domain parameters (if param == 'all', apply the same values to all the parameters) //TODO: Change this appropriately
  //val domainParamValues = loadDomainParams(domainParamKBPath)

  // Load the gradable adj grounding KB file
  val gradableAdjGroundingModel = loadGradableAdjGroundingFile(quantifierKBPath)

  // ODIN components
  val actions = new AgroActions
  val engine: ExtractorEngine = ExtractorEngine(rules, actions)

  // LexiconNER for labeling domain entities
  //TODO: agrovoc lexicons aren't in this project yet
  // todo: the order matters, we should be taking order into account
//  println("agrovoc path: " + agrovocLexiconsPath)
//
//  val agrovocLexicons = findFilesFromResources(agrovocLexiconsPath, "tsv")
//  agrovocLexicons.foreach(println)

  // todo: handle adverbial quantifiers
//  val ner = LexiconNER(
//    Seq("org/clulab/wm/lexicons/Quantifier.tsv", "org/clulab/wm/lexicons/IncDec.tsv") ++ agrovocLexicons,
//    caseInsensitiveMatching = true
//  )

  //TODO: keep Quantifier...
  val ner = LexiconNER(
    Seq("org/clulab/wm/lexicons/Quantifier.tsv"), //, "org/clulab/wm/lexicons/IncDec.tsv"),
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

  def extractEntitiesFrom(doc: Document, state: State = new State()): Vector[Mention] = {
    println ("Extracting from: " + doc.sentences.map(_.getSentenceText()).mkString(" "))
      val foundEntities = entityFinder.extractAndFilter(doc).toVector
      debugPrint("Found (filtered) entities:")
      debugMentions(foundEntities)
      val keptEntities = foundEntities.filter(!_.matches("Avoid"))
      debugPrint("keptEntities:")
      debugMentions(keptEntities)


//      val locallyExtractedEntities = entityEngine.extractFrom(doc, state)
//      val quantifiers = locallyExtractedEntities.filter(m => m.labels.contains("Quantifier"))
//      //val params = locallyExtractedEntities.filter(m => m.labels.contains("Param"))
//      //val agrovoc = locallyExtractedEntities.filter(m => m.labels.contains("AGROVOC"))
//
//      debugPrint("Params:")
//      debugMentions(params)
//      debugPrint("Quantifiers:")
//      debugMentions(quantifiers)
//      debugPrint("AGROVOC:")
//      debugMentions(agrovoc)
//
//      val expandedParams = params.map(p => expandParams(keptEntities, p)).toVector
//      debugPrint("ExpandedParams:")
//      debugMentions(expandedParams)
//
//      val relabeled = actions.copyLabel(expandedParams, State(keptEntities ++ params ++ expandedParams))
//      debugPrint("Relabeled:")
//      debugMentions(relabeled)
//      //    val longestEntities = actions.keepLongestMentions(res, State(res)).toVector
      return keptEntities // ++ relabeled ++ quantifiers ++ agrovoc


  }

  def expandParams(nounPhrases: Seq[Mention], paramMention: Mention): Mention = {
    //val paramTB = paramMention.asInstanceOf[TextBoundMention]
    val nounPhrasesContainingParam = nounPhrases.filter(np => np.label != "Avoid" && np.tokenInterval.contains(paramMention.tokenInterval))
    if (nounPhrasesContainingParam.nonEmpty) {
      val longest = nounPhrasesContainingParam.sortBy(-_.tokenInterval.length).head
      // Save this original text for the grounding model lookup
      val baseParamText = paramMention.text
      val tempParamMap = new mutable.HashMap[String, Seq[Mention]]
      tempParamMap.put("baseParam", Seq(paramMention))
      val newArgsMap = longest.arguments ++ tempParamMap
      val expandedAsRelationMention = new RelationMention(
        labels = paramMention.labels,
        tokenInterval = longest.tokenInterval,
        paths = longest.paths,
        arguments = newArgsMap,
        sentence = paramMention.sentence,
        document = paramMention.document,
        keep = paramMention.keep,
        foundBy = s"${paramMention.foundBy}-${AgroSystem.EXPAND_SUFFIX}"
      )
      return expandedAsRelationMention
//      return longest.asInstanceOf[TextBoundMention].copy(
//        labels = paramTB.labels,
//        foundBy = s"${paramTB.foundBy}-${AgroSystem.EXPAND_SUFFIX}")
    }
    paramMention
  }


  def extractEventsFrom(doc: Document, state: State): Vector[Mention] = {
    val res = engine.extractFrom(doc, state).toVector
//    res
    val cleanMentions = actions.keepMostCompleteEvents(res, State(res)).toVector
//    val longest = actions.keepLongestMentions(cleanMentions, State(cleanMentions)).toVector
//   longest
    cleanMentions
  }

  def extractFrom(doc: Document): Vector[Mention] = {
    // get entities
    val entities = extractEntitiesFrom(doc)
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
  val DEFAULT_DOMAIN_PARAM: String = "all"
  val MU_COEFF: String = "mu_coeff"
  val SIGMA_COEFF: String = "sigma_coeff"
  val INTERCEPT: String = "intercept"
  val PARAM_MEAN: String = "mean"
  val PARAM_STDEV: String = "stdev"



}


