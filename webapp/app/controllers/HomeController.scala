package controllers

import javax.inject._

import agro.demo.RAPShell.GroundedEntity
import play.api._
import play.api.mvc._
import play.api.libs.json._
import org.clulab.processors.{Document, Sentence}
import agro.demo._
import org.clulab.odin.{EventMention, Mention, RelationMention, TextBoundMention}
import org.clulab.sequences.LexiconNER
import org.clulab.wm.AgroSystem

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  // Initialize the AgroSystem
  // -------------------------------------------------
  println("[AgroSystem] Initializing the AgroSystem ...")
  val ieSystem = new AgroSystem()

  var proc = ieSystem.proc
  val ner = LexiconNER(Seq("org/clulab/wm/lexicons/Quantifier.tsv"), caseInsensitiveMatching = true)
  val grounder = ieSystem.grounder
  println("[AgroSystem] Completed Initialization ...")
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

  def parseSentence(sent: String) = Action {
    val (procSentence, agroMentions, groundedEntities, causalEvents) = RAPShell.processPlaySentence(ieSystem, sent) // Call the agro.demo.RAPShell.processPlaySentence
    println(s"Sentence returned from processPlaySentence : ${procSentence.getSentenceText()}")
    val json = mkJson(procSentence, agroMentions, groundedEntities, causalEvents) // we only handle a single sentence
    Ok(json)
  }

  def mkJson(sent: Sentence, mentions: Vector[Mention], groundedEntities: Vector[GroundedEntity], causalEvents: Vector[(String, Map[String, String])] ): JsValue = {
    println("Found mentions (in mkJson):")
    mentions.foreach(utils.DisplayUtils.displayMention)

    val syntaxJsonObj = Json.obj(
        "text" -> sent.getSentenceText(),
        "entities" -> mkJsonFromTokens(sent),
        "relations" -> mkJsonFromDependencies(sent)
      )
    val agroJsonObj = mkJsonForAgro(sent, mentions)
    val groundedAdjObj = mkGroundedObj(groundedEntities, mentions, causalEvents)
    println(s"Grounded Gradable Adj: ")
    println(s"$groundedAdjObj")
    Json.obj(
      "syntax" -> syntaxJsonObj,
      "agroMentions" -> agroJsonObj,
      "groundedAdj" -> groundedAdjObj
    )
  }

  def mkGroundedObj(groundedEntities: Vector[GroundedEntity],
                    mentions: Vector[Mention],
                    causalEvents: Vector[(String, Map[String, String])]): String = {
    var objectToReturn = ""

    if(groundedEntities.size > 0){
      objectToReturn += """<br><br><font size="3" color="cadetblue">Grounded Entities:</font>"""

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

    // Entities
    val entities = mentions.filter(_ matches "Entity")
    if (entities.nonEmpty){
      objectToReturn += """<br><font size="3" color="rebeccapurple">Found Entities:</font><br>"""
      for (entity <- entities) {
        objectToReturn += s"${utils.DisplayUtils.webAppMention(entity)}"
      }
    }


    val events = mentions.filter(_ matches "Event")
    if (events.nonEmpty) {
      objectToReturn += """<font size="3" color="green">Found Events:</font><br>"""
      for (event <- events) {
        objectToReturn += s"${utils.DisplayUtils.webAppMention(event)}"
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

  def mkJsonForAgro(sent: Sentence, mentions: Vector[Mention]): Json.JsValueWrapper = {
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
      "text" -> sent.getSentenceText().replaceAll(" , ", ", "),
      "entities" -> mkJsonFromEntities(entities ++ topLevelTBM, tbMentionToId),
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
      m.label,
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

  def mkJsonFromTokens(sent: Sentence): Json.JsValueWrapper = {
    val toks = sent.words.indices.map(i => mkJsonFromToken(sent, i))
    Json.arr(toks: _*)
  }

  def mkJsonFromToken(sent: Sentence, i: Int): Json.JsValueWrapper = {
    Json.arr(
      s"T${i + 1}", // token id (starts at one, not zero)
      sent.tags.get(i), // lets assume that tags are always available
      Json.arr(Json.arr(sent.startOffsets(i), sent.endOffsets(i)))
    )
  }

  def mkJsonFromDependencies(sent: Sentence): Json.JsValueWrapper = {
    var relId = 0
    val deps = sent.dependencies.get // lets assume that dependencies are always available
    val rels = for {
      governor <- deps.outgoingEdges.indices
      (dependent, label) <- deps.outgoingEdges(governor)
    } yield {
      relId += 1
      mkJsonFromDependency(sent, relId, governor + 1, dependent + 1, label)
    }
    Json.arr(rels: _*)
  }

  def mkJsonFromDependency(sent: Sentence, relId: Int, governor: Int, dependent: Int, label: String): Json.JsValueWrapper = {
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
