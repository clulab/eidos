package org.clulab.wm.eidos

import java.net.URL
import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.odin._
import org.clulab.processors.clu._
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.{Document, Processor, Sentence}
import org.clulab.sequences.LexiconNER
import org.clulab.wm.eidos.attachments.Score
import org.clulab.wm.eidos.attachments.NegationHandler._
import org.clulab.wm.eidos.entities.EidosEntityFinder
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.groundings.Aliases.Groundings
import org.clulab.wm.eidos.groundings.EidosOntologyGrounder.{FAO_NAMESPACE, MESH_NAMESPACE, UN_NAMESPACE, WDI_NAMESPACE}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils._
import ai.lum.common.ConfigUtils._
import org.slf4j.LoggerFactory
import org.clulab.wm.eidos.document.EidosDocument
import org.clulab.timenorm.TemporalCharbasedParser

import scala.annotation.tailrec

case class AnnotatedDocument(var document: Document, var odinMentions: Seq[Mention], var eidosMentions: Seq[EidosMention])

/**
  * A system for text processing and information extraction
  */
class EidosSystem(val config: Config = ConfigFactory.load("eidos")) extends StopwordManaging with MultiOntologyGrounder with AdjectiveGrounder {
  def this(x: Object) = this() // Dummy constructor crucial for Python integration

  val eidosConf = config[Config]("EidosSystem")

  println("Loading processor...")
  val proc: Processor = mkProcessor(eidosConf)

  private def mkProcessor(config: Config): Processor = {
    config[String]("language") match {
      case "english" => new FastNLPProcessor
      case "spanish" => new SpanishCluProcessor
      case "portuguese" => new PortugueseCluProcessor
    }
  }

  var debug = true // Allow external control with var

  println("Loading W2V...")
  var word2vec = eidosConf[Boolean]("useW2V") // Turn this on and off here
  // This isn't intended to be (re)loadable.  This only happens once.
  val wordToVec = EidosWordToVec(
    word2vec,
    LoadableAttributes.wordToVecPath,
    eidosConf[Int]("topKNodeGroundings"),
    LoadableAttributes.cacheDir,
    LoadableAttributes.useCache
  )

  class LoadableAttributes(
    // These are the values which can be reloaded.  Query them for current assignments.
    val entityFinder: EidosEntityFinder,
    val domainParams: DomainParams,
    val adjectiveGrounder: AdjectiveGrounder,
    val actions: EidosActions,
    val engine: ExtractorEngine,
    val ner: LexiconNER,
    val stopwordManager: StopwordManager,
    val ontologyGrounders: Seq[EidosOntologyGrounder],
    val timenorm: Option[TemporalCharbasedParser]
  )

  object LoadableAttributes {
    // Extraction
    def     masterRulesPath: String = eidosConf[String]("masterRulesPath")
    def    quantifierKBPath: String = eidosConf[String]("quantifierKBPath")
    def   domainParamKBPath: String = eidosConf[String]("domainParamKBPath")
    def      quantifierPath: String = eidosConf[String]("quantifierPath")
    def     entityRulesPath: String = eidosConf[String]("entityRulesPath")
    def      avoidRulesPath: String = eidosConf[String]("avoidRulesPath")
    def        taxonomyPath: String = eidosConf[String]("taxonomyPath")
    // Filtering
    def       stopwordsPath: String = eidosConf[String]("stopWordsPath")
    def     transparentPath: String = eidosConf[String]("transparentPath")
    // Ontology handling
    def      unOntologyPath: String = eidosConf[String]("unOntologyPath")
    def     wdiOntologyPath: String = eidosConf[String]("wdiOntologyPath")
    def     faoOntologyPath: String = eidosConf[String]("faoOntologyPath")
    def    meshOntologyPath: String = eidosConf[String]("meshOntologyPath")
    def            cacheDir: String = eidosConf[String]("cacheDir")

    // These are needed to construct some of the loadable attributes even though it isn't a path itself.
    def ontologies: Seq[String] = eidosConf[List[String]]("ontologies")
    def maxHops: Int = eidosConf[Int]("maxHops")
    def      wordToVecPath: String = eidosConf[String]("wordToVecPath")
    def  timeNormModelPath: String = eidosConf[String]("timeNormModelPath")
    def useTimeNorm: Boolean = eidosConf[Boolean]("useTimeNorm")
    def    useCache: Boolean = eidosConf[Boolean]("useCache")

    val stopwordManager = StopwordManager(stopwordsPath, transparentPath)
    val canonicalizer = new Canonicalizer(stopwordManager)

    protected def mkDomainOntology(name: String): DomainOntology = {
      val serializedPath: String = DomainOntologies.serializedPath(name, cacheDir)

      name match {
        case   UN_NAMESPACE =>   UNOntology(  unOntologyPath, serializedPath, proc, canonicalizer, useCache = useCache)
        case  WDI_NAMESPACE =>  WDIOntology( wdiOntologyPath, serializedPath, proc, canonicalizer, useCache = useCache)
        case  FAO_NAMESPACE =>  FAOOntology( faoOntologyPath, serializedPath, proc, canonicalizer, useCache = useCache)
        case MESH_NAMESPACE => MeshOntology(meshOntologyPath, serializedPath, proc, canonicalizer, useCache = useCache)
        case _ => throw new IllegalArgumentException("Ontology " + name + " is not recognized.")
      }
    }

    def apply(): LoadableAttributes = {
      // Odin rules and actions:
      // Reread these values from their files/resources each time based on paths in the config file.
      val masterRules = FileUtils.getTextFromResource(masterRulesPath)
      val actions = EidosActions(taxonomyPath)

      // Domain Ontologies:
      val ontologyGrounders =
          if (word2vec) ontologies.par.map(ontology => EidosOntologyGrounder(ontology, mkDomainOntology(ontology), wordToVec)).seq
          else Seq.empty

      val timenorm: Option[TemporalCharbasedParser] =
          if (!useTimeNorm) None
          else {
            val timeNormResource: URL = getClass.getResource(timeNormModelPath)
            // See https://stackoverflow.com/questions/6164448/convert-url-to-normal-windows-filename-java/17870390
            val file = Paths.get(timeNormResource.toURI()).toFile().getAbsolutePath()
            // timenormResource.getFile() won't work for Windows, probably because Hdf5Archive is
            //     public native void openFile(@StdString BytePointer var1, ...
            // and needs native representation of the file.
            Some(new TemporalCharbasedParser(file))
          }

      new LoadableAttributes(
        EidosEntityFinder(entityRulesPath, avoidRulesPath, maxHops = maxHops),
        DomainParams(domainParamKBPath),
        EidosAdjectiveGrounder(quantifierKBPath),
        actions,
//        ExtractorEngine(masterRules, actions, actions.mergeAttachments), // ODIN component
        ExtractorEngine(masterRules, actions, actions.globalAction), // ODIN component
        LexiconNER(Seq(quantifierPath), caseInsensitiveMatching = true), //TODO: keep Quantifier...
        stopwordManager,
        ontologyGrounders,
        timenorm
      )
    }
  }

  println("Loading loadableAttributes...")
  var loadableAttributes = LoadableAttributes()

  // These public variables are accessed directly by clients which
  // don't know they are loadable and which had better not keep copies.
  def domainParams = loadableAttributes.domainParams
  def engine = loadableAttributes.engine
  def ner = loadableAttributes.ner
  def timenorm = loadableAttributes.timenorm

  def reload() = loadableAttributes = LoadableAttributes()

  // Annotate the text using a Processor and then populate lexicon labels
  def annotate(text: String, keepText: Boolean = true, documentCreationTime: Option[String] = None, filename: Option[String]= None): Document = {
    val oldDoc = proc.annotate(text, true) // Formerly keepText, must now be true
    val doc = EidosDocument(oldDoc, keepText, documentCreationTime)
    doc.sentences.foreach(addLexiconNER)
    doc.parseTime(loadableAttributes.timenorm)
    doc.id = filename
    doc
  }

  protected def addLexiconNER(s: Sentence) = {
    // The Portuguese parser does not currently generate entities, so we want to create an empty list here for
    // further processing and filtering operations that expect to be able to query the entities
    if (s.entities.isEmpty) s.entities = Some(Array.fill[String](s.words.length)("O"))
    for {
      (lexiconNERTag, i) <- ner.find(s).zipWithIndex
      if lexiconNERTag != EidosSystem.NER_OUTSIDE
    } s.entities.get(i) = lexiconNERTag

  }

  // MAIN PIPELINE METHOD
  def extractFromText(text: String, keepText: Boolean = true, cagRelevantOnly: Boolean = true,
                      documentCreationTime: Option[String] = None, filename: Option[String] = None): AnnotatedDocument = {
    val doc = annotate(text, keepText, documentCreationTime, filename)
    val odinMentions = extractFrom(doc)
    // Dig in and get any Mentions that currently exist only as arguments, so that they get to be part of the state
    @tailrec
    def traverse(ms: Seq[Mention], results: Seq[Mention], seen: Set[Mention]): Seq[Mention] = {
      ms match {
        case Nil => results
        case m +: rest if !seen.contains(m) =>
          //DisplayUtils.shortDisplay(m)
          val args = m.arguments.values.flatten
          traverse(rest ++ args, m +: results, seen + m)
        case m +: rest => traverse(rest, results, seen)
      }
    }

    val mentionsAndNestedArgs = traverse(odinMentions, Seq.empty, Set.empty)


    //println(s"\nodinMentions() -- entities : \n\t${odinMentions.map(m => m.text).sorted.mkString("\n\t")}")
    val cagRelevant = if (cagRelevantOnly) keepCAGRelevant(mentionsAndNestedArgs) else mentionsAndNestedArgs
    val eidosMentions = EidosMention.asEidosMentions(cagRelevant, new Canonicalizer(loadableAttributes.stopwordManager), this)

    new AnnotatedDocument(doc, cagRelevant, eidosMentions)
  }

  def extractEventsFrom(doc: Document, state: State): Vector[Mention] = {
    val res = engine.extractFrom(doc, state).toVector
    loadableAttributes.actions.keepMostCompleteEvents(res, State(res)).toVector
  }

  def extractFrom(doc: Document): Vector[Mention] = {
    // get entities
    val entities: Seq[Mention] = loadableAttributes.entityFinder.extractAndFilter(doc).toVector
//    println("Entities that made it to EidosSystem:")
//    entities.foreach(e => DisplayUtils.shortDisplay(e))
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
    def sameAs(a: Mention, b: Mention, score: Float): Mention = {
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
    val allMentions = State(mentions)
    val cagEdgeMentions = mentions.filter(m => releventEdge(m, allMentions))

    // Should these be included as well?

    // 3) These last ones may overlap with the above or include mentions not in the original list.
    val cagEdgeArguments = cagEdgeMentions.flatMap(mention => mention.arguments.values.flatten.toSeq)
    // Put them all together.
    val releventEdgesAndTheirArgs = cagEdgeMentions ++ cagEdgeArguments
    // To preserve order, avoid repeats, and not allow anything new in the list, filter the original.
    mentions.filter(mention => isCAGRelevant(mention, cagEdgeMentions, cagEdgeArguments))
  }

  def isCAGRelevant(mention: Mention, cagEdgeMentions: Seq[Mention], cagEdgeArguments: Seq[Mention]): Boolean =
    // We're no longer keeping all modified entities
    //(mention.matches("Entity") && mention.attachments.nonEmpty) ||
      cagEdgeMentions.contains(mention) ||
      cagEdgeArguments.contains(mention)

  def releventEdge(m: Mention, state: State): Boolean = {
    m match {
      case tb: TextBoundMention => EidosSystem.CAG_EDGES.contains(tb.label)
      case rm: RelationMention => EidosSystem.CAG_EDGES.contains(rm.label)
      case em: EventMention => EidosSystem.CAG_EDGES.contains(em.label) && argumentsHaveContent(em, state)
      case cs: CrossSentenceMention => EidosSystem.CAG_EDGES.contains(cs.label)
      case _ => throw new UnsupportedClassVersionError()
    }
  }

  def argumentsHaveContent(mention: EventMention, state: State): Boolean = {
    val causes: Seq[Mention] = mention.arguments.getOrElse("cause", Seq.empty)
    val effects: Seq[Mention] = mention.arguments.getOrElse("effect", Seq.empty)

    if (causes.nonEmpty && effects.nonEmpty) // If it's something interesting,
    // then both causes and effects should have some content
      causes.exists(loadableAttributes.stopwordManager.hasContent(_, state)) && effects.exists(loadableAttributes.stopwordManager.hasContent(_, state))
    else
      true
  }

  /*
      Grounding
  */

  def containsStopword(stopword: String) =
    loadableAttributes.stopwordManager.containsStopword(stopword)

  def groundOntology(mention: EidosMention): Groundings =
      loadableAttributes.ontologyGrounders.map (ontologyGrounder =>
        (ontologyGrounder.name, ontologyGrounder.groundOntology(mention))).toMap

  def groundAdjective(quantifier: String): AdjectiveGrounding =
    loadableAttributes.adjectiveGrounder.groundAdjective(quantifier)

  /*
      Wrapper for using w2v on some strings
   */
  def stringSimilarity(string1: String, string2: String): Float = wordToVec.stringSimilarity(string1, string2)

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

  // Taxonomy relations that should make it to final causal analysis graph
  val CAUSAL_LABEL: String = "Causal"
  val CORR_LABEL: String = "Correlation"
  val COREF_LABEL: String = "Coreference"
  // Taxonomy relations for other uses
  val RELATION_LABEL: String = "EntityLinker"

  // Stateful Labels used by webapp
  val INC_LABEL_AFFIX = "-Inc"
  val DEC_LABEL_AFFIX = "-Dec"
  val QUANT_LABEL_AFFIX = "-Quant"
  val NER_OUTSIDE = "O"
  // Provenance info for sameAs scoring
  val SAME_AS_METHOD = "simple-w2v"

  // CAG filtering
  val CAG_EDGES = Set(CAUSAL_LABEL, CORR_LABEL, COREF_LABEL)
}
