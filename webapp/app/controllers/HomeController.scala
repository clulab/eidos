package controllers

import javax.inject._

import org.clulab.odin.{Attachment, EventMention, Mention, RelationMention, TextBoundMention}
import org.clulab.processors.{Document, Sentence}
import org.clulab.sequences.LexiconNER
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.Aliases._
import org.clulab.wm.eidos.utils.DisplayUtils

import play.api._
import play.api.mvc._
import play.api.libs.json._

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

  case class GroundedEntity(sentence: String,
                            quantifier: Quantifier,
                            entity: Entity,
                            predictedDelta: Option[Double],
                            mean: Option[Double],
                            stdev: Option[Double])



  // Return the sorted Quantification, Increase, and Decrease modifications
  def separateAttachments(m: Mention): (Set[Attachment], Set[Attachment], Set[Attachment]) = {
    val attachments = m.attachments
    (attachments.filter(_.isInstanceOf[Quantification]),
      attachments.filter(_.isInstanceOf[Increase]),
      attachments.filter(_.isInstanceOf[Decrease]))
  }
  
  def groundEntity(mention: Mention, quantifier: Quantifier, ieSystem: EidosSystem): GroundedEntity = {
    val grounding = ieSystem.ground(mention, quantifier)

    // add the calculation
    println("loaded domain params:" + ieSystem.domainParamValues.toString())
    println(s"\tkeys: ${ieSystem.domainParamValues.keys.mkString(", ")}")
    println(s"getting details for: ${mention.text}")

    val paramDetails = ieSystem.domainParamValues.get("DEFAULT").get
    val paramMean = paramDetails.get(EidosSystem.PARAM_MEAN)
    val paramStdev = paramDetails.get(EidosSystem.PARAM_STDEV)
    val predictedDelta =
        if (!grounding.isGrounded) None
        else Some(math.pow(math.E, grounding.intercept.get + (grounding.mu.get * paramMean.get) + (grounding.sigma.get * paramStdev.get)) * paramStdev.get)

    GroundedEntity(mention.document.sentences(mention.sentence).getSentenceText(), quantifier, mention.text, predictedDelta, grounding.mu, grounding.sigma)
  }


  def groundEntities(ieSystem: EidosSystem, mentions: Seq[Mention]): Vector[GroundedEntity] = {
    val gms = for {
      m <- mentions
      (quantifications, increases, decreases) = separateAttachments(m)

      groundedQuantifications = for {
        q <- quantifications
        quantTrigger = q.asInstanceOf[Quantification].quantifier
      } yield groundEntity(m, quantTrigger, ieSystem)

      groundedIncreases = for {
        inc <- increases
        quantTrigger <- inc.asInstanceOf[Increase].quantifiers.getOrElse(Seq.empty[Quantifier])
      } yield groundEntity(m, quantTrigger, ieSystem)

      groundedDecreases = for {
        dec <- decreases
        quantTrigger <- dec.asInstanceOf[Decrease].quantifiers.getOrElse(Seq.empty[Quantifier])
      } yield groundEntity(m, quantTrigger, ieSystem)


      } yield groundedQuantifications ++ groundedIncreases ++ groundedDecreases

    gms.flatten.toVector
  }
  
  

  def getEntityLinkerEvents(mentions: Vector[Mention]): Vector[(Trigger, Map[String, String])] = {
    val events = mentions.filter(_ matches "Event")
    val entityLinkingEvents = events.filter(_ matches "EntityLinker").map { e =>
      val event = e.asInstanceOf[EventMention]
      val trigger = event.trigger.text
      val arguments = event.arguments.map { a =>
        val name = a._1
        val arg_mentions = a._2.map(_.text).mkString(" ")
        (name, arg_mentions)
      }
      (trigger, arguments)
    }

    entityLinkingEvents
  }
  
  def processPlaySentence(ieSystem: EidosSystem, text: String): (Sentence, Vector[Mention], Vector[GroundedEntity], Vector[(Trigger, Map[String, String])]) = {
    // preprocessing
    println(s"Processing sentence : ${text}" )
    val doc = ieSystem.annotate(text)

    println(s"DOC : ${doc}")
    // extract mentions from annotated document
    val mentions = ieSystem.extractFrom(doc).sortBy(m => (m.sentence, m.getClass.getSimpleName))
    println(s"Done extracting the mentions ... ")
    println(s"They are : ${mentions.map(m => m.text).mkString(",\t")}")

    println(s"Grounding the gradable adjectives ... ")
    val groundedEntities = groundEntities(ieSystem, mentions)

    println(s"Getting entity linking events ... ")
    val events = getEntityLinkerEvents(mentions)

    println("DONE .... ")
//    println(s"Grounded Adjectives : ${groundedAdjectives.size}")
    // return the sentence and all the mentions extracted ... TODO: fix it to process all the sentences in the doc
    (doc.sentences.head, mentions.sortBy(_.start), groundedEntities, events)
  }
  
  def parseSentence(sent: String) = Action {
    val (procSentence, agroMentions, groundedEntities, causalEvents) = processPlaySentence(ieSystem, sent)
    println(s"Sentence returned from processPlaySentence : ${procSentence.getSentenceText()}")
    val json = mkJson(sent, procSentence, agroMentions, groundedEntities, causalEvents) // we only handle a single sentence
    Ok(json)
  }

  def mkJson(sentenceText: String, sent: Sentence, mentions: Vector[Mention], groundedEntities: Vector[GroundedEntity], causalEvents: Vector[(String, Map[String, String])] ): JsValue = {
    println("Found mentions (in mkJson):")
    mentions.foreach(DisplayUtils.displayMention)

    val syntaxJsonObj = Json.obj(
        "text" -> sentenceText,
        "entities" -> mkJsonFromTokens(sent),
        "relations" -> mkJsonFromDependencies(sent)
      )
    val agroJsonObj = mkJsonForAgro(sentenceText, sent, mentions)
    val groundedAdjObj = mkGroundedObj(groundedEntities, mentions, causalEvents)

    // These print the html and it's a mess to look at...
    // println(s"Grounded Gradable Adj: ")
    // println(s"$groundedAdjObj")
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
      objectToReturn += s"""<br><br><font size="4" color="${HomeController.sectionTitleColor}">Grounded Entities:</font>"""

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
      objectToReturn += s"""<br><font size="4" color="${HomeController.sectionTitleColor}">Found Entities:</font><br>"""
      for (entity <- entities) {
        objectToReturn += s"${DisplayUtils.webAppMention(entity)}"
      }
    }


    val events = mentions.filter(_ matches "Event")
    if (events.nonEmpty) {
      objectToReturn += s"""<font size="4" color="${HomeController.sectionTitleColor}">Found Events:</font><br>"""
      for (event <- events) {
        objectToReturn += s"${DisplayUtils.webAppMention(event)}"
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

  def mkJsonForAgro(sentenceText: String, sent: Sentence, mentions: Vector[Mention]): Json.JsValueWrapper = {
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

object HomeController {

  val sectionTitleColor = "#2471A3"

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
