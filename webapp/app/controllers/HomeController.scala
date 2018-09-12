package controllers

import javax.inject._
import org.clulab.odin._
import org.clulab.processors.{Document, Sentence}
import org.clulab.sequences.LexiconNER
import org.clulab.struct.DirectedGraph
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.BuildInfo
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.Aliases._
import org.clulab.wm.eidos.utils.DisplayUtils
import org.clulab.wm.eidos.utils.DomainParams
import org.clulab.wm.eidos.document.EidosDocument
import org.clulab.wm.eidos.document.TimeInterval
import java.time.LocalDateTime
import org.clulab.wm.eidos.groundings.{DomainOntology, EidosOntologyGrounder, OntologyGrounding}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.{DisplayUtils, DomainParams, GroundingUtils}
import play.api._
import play.api.mvc._
import play.api.libs.json._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  // Initialize the EidosSystem
  // -------------------------------------------------
  println("[EidosSystem] Initializing the EidosSystem ...")
  val ieSystem = new EidosSystem()
  var proc = ieSystem.proc
  println("[EidosSystem] Completed Initialization ...")
  // -------------------------------------------------

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def buildInfo = Action {
    Ok(jsonBuildInfo)
  }

  // Entry method
  def parseSentence(text: String, cagRelevantOnly: Boolean) = Action {
    val (doc, eidosMentions, groundedEntities, causalEvents) = processPlaySentence(ieSystem, text, cagRelevantOnly)
    println(s"Sentence returned from processPlaySentence : ${doc.sentences.head.getSentenceText}")
    val json = mkJson(text, doc, eidosMentions, groundedEntities, causalEvents) // we only handle a single sentence
    Ok(json)
  }

  // Method where eidos happens!
  def processPlaySentence(
    ieSystem: EidosSystem,
    text: String,
    cagRelevantOnly: Boolean): (Document, Vector[EidosMention], Vector[GroundedEntity], Vector[(Trigger, Map[String, String])]) = {

    def mentionOrder(m: Mention): Int = 10000 * m.sentence + m.start

    // preprocessing
    println(s"Processing sentence : ${text}" )
    val doc = ieSystem.annotate(text)

    // Debug
    println(s"DOC : ${doc}")
    // extract mentions from annotated document
    val annotatedDocument = ieSystem.extractFromText(text, cagRelevantOnly = cagRelevantOnly)
    val mentions = annotatedDocument.eidosMentions.sortBy(m => (m.odinMention.sentence, m.getClass.getSimpleName)).toVector


    println(s"Done extracting the mentions ... ")
    println(s"They are : ${mentions.map(m => m.odinMention.text).mkString(",\t")}")

    println(s"Grounding the gradable adjectives ... ")
    val groundedEntities = groundEntities(ieSystem, mentions)

    println(s"Getting entity linking events ... ")
    val events = getEntityLinkerEvents(mentions)

    println("DONE .... ")
    //    println(s"Grounded Adjectives : ${groundedAdjectives.size}")
    // return the sentence and all the mentions extracted ... TODO: fix it to process all the sentences in the doc
    (doc, mentions.sortBy(m => mentionOrder(m.odinMention)), groundedEntities, events)
  }


  case class GroundedEntity(sentence: String,
                            quantifier: Quantifier,
                            entity: Entity,
                            predictedDelta: Option[Double],
                            mean: Option[Double],
                            stdev: Option[Double])



  // Return the sorted Quantification, Increase, and Decrease modifications
  def separateAttachments(m: EidosMention): (Set[Attachment], Set[Attachment], Set[Attachment]) = {
    val attachments = m.odinMention.attachments
    (attachments.filter(_.isInstanceOf[Quantification]),
      attachments.filter(_.isInstanceOf[Increase]),
      attachments.filter(_.isInstanceOf[Decrease]))
  }
  
  def groundEntity(mention: Mention, quantifier: String, ieSystem: EidosSystem): GroundedEntity = {
    // add the calculation
    println("loaded domain params:" + ieSystem.domainParams.toString())
    println(s"\tkeys: ${ieSystem.domainParams.keys.mkString(", ")}")
    println(s"getting details for: ${mention.text}")

    val paramDetails = ieSystem.domainParams.get(DomainParams.DEFAULT_DOMAIN_PARAM).get
    val paramMean = paramDetails.get(DomainParams.PARAM_MEAN).get
    val paramStdev = paramDetails.get(DomainParams.PARAM_STDEV).get
    val grounding = ieSystem.groundAdjective(quantifier)
    val predictedDelta = grounding.predictDelta(paramMean, paramStdev)

    GroundedEntity(mention.document.sentences(mention.sentence).getSentenceText, quantifier, mention.text, predictedDelta, grounding.mu, grounding.sigma)
  }


  def groundEntities(ieSystem: EidosSystem, mentions: Seq[EidosMention]): Vector[GroundedEntity] = {
    val gms = for {
      m <- mentions
      (quantifications, increases, decreases) = separateAttachments(m)

      groundedQuantifications = for {
        q <- quantifications
        quantTrigger = q.asInstanceOf[Quantification].trigger
      } yield groundEntity(m.odinMention, quantTrigger, ieSystem)

      groundedIncreases = for {
        inc <- increases
        quantTrigger <- inc.asInstanceOf[Increase].quantifiers.getOrElse(Seq.empty[Quantifier])
      } yield groundEntity(m.odinMention, quantTrigger, ieSystem)

      groundedDecreases = for {
        dec <- decreases
        quantTrigger <- dec.asInstanceOf[Decrease].quantifiers.getOrElse(Seq.empty[Quantifier])
      } yield groundEntity(m.odinMention, quantTrigger, ieSystem)


      } yield groundedQuantifications ++ groundedIncreases ++ groundedDecreases

    gms.flatten.toVector
  }
  
  

  def getEntityLinkerEvents(mentions: Vector[EidosMention]): Vector[(Trigger, Map[String, String])] = {
    val events = mentions.filter(_.odinMention matches "Event")
    val entityLinkingEvents = events.filter(_.odinMention matches "EntityLinker").map { e =>
      e.odinMention match {
        case em: EventMention =>
          val event = e.odinMention.asInstanceOf[EventMention]
          val trigger = event.trigger.text
          val arguments = event.arguments.map { a =>
            val name = a._1
            val arg_mentions = a._2.map(_.text).mkString(" ")
            (name, arg_mentions)
          }
          (trigger, arguments)
        case cs: CrossSentenceMention =>
          val arguments = cs.arguments.map { a =>
            val name = a._1
            val arg_mentions = a._2.map(_.text).mkString(" ")
            (name, arg_mentions)
          }
          ("coref", arguments)
        case _ => throw new RuntimeException("Unexpected event Mention type!")
      }

    }

    entityLinkingEvents
  }

  val jsonBuildInfo: JsValue = Json.obj(
    "name" -> BuildInfo.name,
    "version" -> BuildInfo.version,
    "scalaVersion" -> BuildInfo.scalaVersion,
    "sbtVersion" -> BuildInfo.sbtVersion,
    "libraryDependencies" -> BuildInfo.libraryDependencies,
    "scalacOptions" -> BuildInfo.scalacOptions,
    "gitCurrentBranch" -> BuildInfo.gitCurrentBranch,
    "gitHeadCommit" -> BuildInfo.gitHeadCommit,
    "gitHeadCommitDate" -> BuildInfo.gitHeadCommitDate,
    "gitUncommittedChanges" -> BuildInfo.gitUncommittedChanges,
    "builtAtString" -> BuildInfo.builtAtString,
    "builtAtMillis" -> BuildInfo.builtAtMillis
  )

  protected def mkParseObj(sentence: Sentence, sb: StringBuilder): Unit = {

    def getTd(text: String): String = "<td>" + xml.Utility.escape(text) + "</td>"

    def getTdAtOptString(option: Option[Array[String]], n: Int): String = {
      val text =
          if (option.isEmpty) ""
          else option.get(n)

      getTd(text)
    }

    def getTdAtString(values: Array[String], n: Int): String = getTd(values(n))

    def getTdAtInt(values: Array[Int], n: Int): String = getTd(values(n).toString)


    def edgesToString(to: Int): String = {
      val edges = sentence.dependencies.get.incomingEdges(to)

      edges.map(edge => sentence.words(edge._1) + "==" + edge._2 + "=>" + sentence.words(to)).mkString(", ")
    }

    sentence.words.indices.foreach { i =>
      sb
        .append("<tr>")
        .append(getTdAtString(sentence.raw, i))
        .append(getTdAtInt(sentence.startOffsets, i))
        .append(getTdAtInt(sentence.endOffsets, i))
        .append(getTdAtString(sentence.words, i))
        .append(getTdAtOptString(sentence.tags, i))
        .append(getTdAtOptString(sentence.lemmas, i))
        .append(getTdAtOptString(sentence.entities, i))
        .append(getTdAtOptString(sentence.norms, i))
        .append(getTdAtOptString(sentence.chunks, i))
        .append(getTd(edgesToString(i)))
        .append("</tr>")
    }
  }

  protected def mkParseObj(doc: Document): String = {
      val header =
      """
        |  <tr>
        |    <th>Text</th>
        |    <th>Start</th>
        |    <th>End</th>
        |    <th>Word</th>
        |    <th>Tags</th>
        |    <th>Lemmas</th>
        |    <th>Entities</th>
        |    <th>Norms</th>
        |    <th>Chunks</th>
        |    <th>Dependencies</th>
        |  </tr>
      """.stripMargin
      val sb = new StringBuilder(header)

      doc.sentences.indices.foreach{ i =>
        val sentence = doc.sentences(i)

        sb.append(s"<tr><td colspan='10' align='center'>Sentence ${i + 1}, sentence.equivalenceHash = ${sentence.equivalenceHash}, dependencies.equivalenceHash = ${sentence.dependencies.get.equivalenceHash}</td></tr>")
        mkParseObj(sentence, sb)
    }
      sb.toString
  }

  def mkJson(text: String, doc: Document, eidosMentions: Vector[EidosMention], groundedEntities: Vector[GroundedEntity], causalEvents: Vector[(String, Map[String, String])] ): JsValue = {
    println("Found mentions (in mkJson):")
    eidosMentions.foreach(eidosMention => DisplayUtils.displayMention(eidosMention.odinMention))

    val sent = doc.sentences.head
    val syntaxJsonObj = Json.obj(
        "text" -> text,
        "entities" -> mkJsonFromTokens(doc),
        "relations" -> mkJsonFromDependencies(doc)
      )
    val eidosJsonObj = mkJsonForEidos(text, sent, eidosMentions.map(_.odinMention), doc.asInstanceOf[EidosDocument].times)
    val groundedAdjObj = mkGroundedObj(groundedEntities, eidosMentions, causalEvents, doc.asInstanceOf[EidosDocument].times)
    val parseObj = mkParseObj(doc)

    // These print the html and it's a mess to look at...
    // println(s"Grounded Gradable Adj: ")
    // println(s"$groundedAdjObj")
    Json.obj(
      "syntax" -> syntaxJsonObj,
      "eidosMentions" -> eidosJsonObj,
      "groundedAdj" -> groundedAdjObj,
      "parse" -> parseObj
    )
  }

  def mkGroundedObj(groundedEntities: Vector[GroundedEntity],
    mentions: Vector[EidosMention],
    causalEvents: Vector[(String, Map[String, String])],
    time: Array[List[TimeInterval]]): String = {
    var objectToReturn = ""

    if(groundedEntities.size > 0){
      objectToReturn += "<h2>Grounded Concepts:</h2>"

      // Make the string for each grounded entity
      val toPrint = for (grounding <- groundedEntities) yield {
        val sentence = grounding.sentence
        val quantifier = grounding.quantifier
        val groundedEntity = grounding.entity
        val predictedDelta = grounding.predictedDelta
        val mean = grounding.mean
        val stdev = grounding.stdev
        var stringToYield = s"${tab}Sentence: ${sentence}"

        stringToYield += s"<br>${tab}Entity: ${groundedEntity}"
        stringToYield += s"<br>${tab}Quantifier: ${quantifier}"
        if (predictedDelta.isDefined && mean.isDefined && stdev.isDefined)
          stringToYield += s"<br>${tab}Predicted delta = ${"%3.3f".format(predictedDelta.get)} (with typical mean=${mean.get} and stdev=${stdev.get})"
        stringToYield += "<br>"
        stringToYield
      }

      toPrint.foreach(str => objectToReturn += s"<br>$str")
    }

    else
      objectToReturn += ""

    // TimeExpressions
    objectToReturn += "<h2>Found TimeExpressions:</h2>"
    for (t <-time) {
      objectToReturn += s"${DisplayUtils.webAppTimeExpressions(t)}"
    }

    // Concepts
    val entities = mentions.filter(_.odinMention matches "Entity")
    if (entities.nonEmpty){
      objectToReturn += "<h2>Found Concepts:</h2>"
      for (entity <- entities) {
        objectToReturn += s"${DisplayUtils.webAppMention(entity.odinMention)}"
        // If the UN groundings are available, let's print them too...
        if (entity.grounding.contains(EidosOntologyGrounder.UN_NAMESPACE)) {
          objectToReturn += s"${DisplayUtils.htmlTab}OntologyLinkings:<br>${DisplayUtils.htmlTab}${DisplayUtils.htmlTab}"
          val groundings = GroundingUtils.getGroundingsString(entity, EidosOntologyGrounder.UN_NAMESPACE, 5, s"<br>${DisplayUtils.htmlTab}${DisplayUtils.htmlTab}")
          objectToReturn +=  groundings
          objectToReturn += "<br><br>"
        }
      }
    }

    // Relations
    val events = mentions.filter(_.odinMention matches "Event")
    if (events.nonEmpty) {
      objectToReturn += s"<h2>Found Relations:</h2>"
      for (event <- events) {
        objectToReturn += s"${DisplayUtils.webAppMention(event.odinMention)}"
      }
    }

//    if(causalEvents.size > 0) {
//      objectToReturn += s"<br><br>EntityLinking Events<br>"
//      for (ce <- causalEvents) {
//        objectToReturn += s"${tab} Trigger : ${ce._1}<br>${tab} Arguments:<br>"
//        for (arg <-  ce._2) {
//          objectToReturn += s"${tab}${tab}${arg._1} => ${arg._2}<br>"
//        }
//      }
//    }

    objectToReturn += "<br>"
    objectToReturn
  }

  def mkJsonForEidos(sentenceText: String, sent: Sentence, mentions: Vector[Mention], time: Array[List[TimeInterval]]): Json.JsValueWrapper = {
    val topLevelTBM = mentions.flatMap {
      case m: TextBoundMention => Some(m)
      case _ => None
    }
    // collect event mentions for display
    val events = mentions.flatMap {
      case m: EventMention => Some(m)
      case _ => None
    }
    // collect triggers for event mentions
    val triggers = events.flatMap { e =>
      val argTriggers = for {
        a <- e.arguments.values
        if a.isInstanceOf[EventMention]
      } yield a.asInstanceOf[EventMention].trigger
      e.trigger +: argTriggers.toSeq
    }
    // collect event arguments as text bound mentions
    val entities = for {
      e <- events
      a <- e.arguments.values.flatten
    } yield a match {
      case m: TextBoundMention => m
      case m: RelationMention => new TextBoundMention(m.labels, m.tokenInterval, m.sentence, m.document, m.keep, m.foundBy)
      case m: EventMention => m.trigger
    }
    // generate id for each textbound mention
    val tbMentionToId = (entities ++ triggers ++ topLevelTBM)
        .distinct
      .zipWithIndex
      .map { case (m, i) => (m, i + 1) }
      .toMap
    // return brat output
    Json.obj(
      "text" -> sentenceText,
      "entities" -> mkJsonFromEntities(entities ++ topLevelTBM, tbMentionToId),
      "timexs" -> mkJsonFromTimeExpressions(time),
      "triggers" -> mkJsonFromEntities(triggers, tbMentionToId),
      "events" -> mkJsonFromEventMentions(events, tbMentionToId)
    )
  }

  def mkJsonFromEntities(mentions: Vector[TextBoundMention], tbmToId: Map[TextBoundMention, Int]): Json.JsValueWrapper = {
    val entities = mentions.map(m => mkJsonFromTextBoundMention(m, tbmToId(m)))
    Json.arr(entities: _*)
  }

  def mkJsonFromTextBoundMention(m: TextBoundMention, i: Int): Json.JsValueWrapper = {
    Json.arr(
      s"T$i",
      HomeController.statefulRepresentation(m).label,
      Json.arr(Json.arr(m.startOffset, m.endOffset))
    )
  }

  def mkJsonFromEventMentions(ee: Seq[EventMention], tbmToId: Map[TextBoundMention, Int]): Json.JsValueWrapper = {
    var i = 0
    val jsonEvents = for (e <- ee) yield {
      i += 1
      mkJsonFromEventMention(e, i, tbmToId)
    }
    Json.arr(jsonEvents: _*)
  }

  def mkJsonFromEventMention(ev: EventMention, i: Int, tbmToId: Map[TextBoundMention, Int]): Json.JsValueWrapper = {
    Json.arr(
      s"E$i",
      s"T${tbmToId(ev.trigger)}",
      Json.arr(mkArgMentions(ev, tbmToId): _*)
    )
  }

  def mkJsonFromTimeExpressions(time: Array[List[TimeInterval]]): Json.JsValueWrapper = {
    var x = 0
    val timexs = for (t <- time; i <- t) yield {
      x += 1
      Json.arr(
        s"X$x",
        "TimeExpression",
        Json.arr(Json.arr(i.span._1,i.span._2)),
        Json.toJson(for(d <- i.intervals) yield ((
          d._1 match {
          case null => "Undef"
          case start => start.toString},
          d._2 match {
          case null => "Undef"
          case end => end.toString},
          d._3)))
      )}
    Json.toJson(timexs)
  }


  def mkArgMentions(ev: EventMention, tbmToId: Map[TextBoundMention, Int]): Seq[Json.JsValueWrapper] = {
    val args = for {
      argRole <- ev.arguments.keys
      m <- ev.arguments(argRole)
    } yield {
      val arg = m match {
        case m: TextBoundMention => m
        case m: RelationMention => new TextBoundMention(m.labels, m.tokenInterval, m.sentence, m.document, m.keep, m.foundBy)
        case m: EventMention => m.trigger
      }
      mkArgMention(argRole, s"T${tbmToId(arg)}")
    }
    args.toSeq
  }

  def mkArgMention(argRole: String, id: String): Json.JsValueWrapper = {
    Json.arr(argRole, id)
  }

  def mkJsonFromTokens(doc: Document): Json.JsValueWrapper = {
    var offset = 0

    val tokens = doc.sentences.flatMap { sent =>
      val tokens = sent.words.indices.map(i => mkJsonFromToken(sent, offset, i))
      offset += sent.words.size
      tokens
    }
    Json.arr(tokens: _*)
  }

  def mkJsonFromToken(sent: Sentence, offset: Int, i: Int): Json.JsValueWrapper = {
    Json.arr(
      s"T${offset + i + 1}", // token id (starts at one, not zero)
      sent.tags.get(i), // lets assume that tags are always available
      Json.arr(Json.arr(sent.startOffsets(i), sent.endOffsets(i)))
    )
  }

  def mkJsonFromDependencies(doc: Document): Json.JsValueWrapper = {
    var offset = 1

    val rels = doc.sentences.flatMap { sent =>
      var relId = 0
      val deps = sent.dependencies.get // lets assume that dependencies are always available
      val rels = for {
        governor <- deps.outgoingEdges.indices
        (dependent, label) <- deps.outgoingEdges(governor)
      } yield {
        val json = mkJsonFromDependency(offset + relId, offset + governor, offset + dependent, label)
        relId += 1
        json
      }
      offset += sent.words.size
      rels
    }
    Json.arr(rels: _*)
  }

  def mkJsonFromDependency(relId: Int, governor: Int, dependent: Int, label: String): Json.JsValueWrapper = {
    Json.arr(
      s"R$relId",
      label,
      Json.arr(
        Json.arr("governor", s"T$governor"),
        Json.arr("dependent", s"T$dependent")
      )
    )
  }

  def tab():String = "&nbsp;&nbsp;&nbsp;&nbsp;"
}

object HomeController {

  // fixme: ordering/precedence...
  def statefulRepresentation(m: Mention): Mention = {
    val stateAffix = m.attachments match {
      case inc if inc.exists(a => a.isInstanceOf[Increase]) => EidosSystem.INC_LABEL_AFFIX
      case dec if dec.exists(a => a.isInstanceOf[Decrease]) => EidosSystem.DEC_LABEL_AFFIX
      case quant if quant.exists(a => a.isInstanceOf[Quantification]) => EidosSystem.QUANT_LABEL_AFFIX
      case _ => ""
    }

    // If you found something, append the affix to top label and add to the Seq of labels
    if (stateAffix.nonEmpty) {
      val modifiedLabels = Seq(m.label ++ stateAffix) ++ m.labels
      val out = m match {
        case tb: TextBoundMention => m.asInstanceOf[TextBoundMention].copy(labels = modifiedLabels)
        case rm: RelationMention => m.asInstanceOf[RelationMention].copy(labels = modifiedLabels)
        case em: EventMention => em.copy(labels = modifiedLabels)
      }

      return out
    }

    // otherwise, return original
    m
  }

}
