package org.clulab.wm.eidos.serialization.html

import org.clulab.odin.CrossSentenceMention
import org.clulab.odin.EventMention
import org.clulab.odin.Mention
import org.clulab.odin.RelationMention
import org.clulab.odin.TextBoundMention
import org.clulab.processors.Sentence
import org.clulab.wm.eidos.attachments.Decrease
import org.clulab.wm.eidos.attachments.Increase
import org.clulab.wm.eidos.attachments.NegChange
import org.clulab.wm.eidos.attachments.PosChange
import org.clulab.wm.eidos.attachments.Quantification
import org.clulab.wm.eidos.context.GeoPhraseID
import org.clulab.wm.eidos.context.TimEx
import play.api.libs.json.Json

object EidosObj {

  def mkJsonForEidos(sentenceText: String, sent: Sentence, mentions: Seq[Mention], time: Option[Array[Seq[TimEx]]], location: Option[Array[Seq[GeoPhraseID]]]): Json.JsValueWrapper = {
    val topLevelTBM = mentions.collect { case m: TextBoundMention => m }

    // collect event mentions for display
    val events = mentions.collect { case m: EventMention => m }

    // collect relation mentions for display
    val relations = mentions.collect { case m: RelationMention => m }

    // collect triggers for event mentions
    val triggers: Seq[TextBoundMention] = events.flatMap { e =>
      val argTriggers = e.arguments.values.flatten.collect { case m: EventMention => m.trigger }
      e.trigger +: argTriggers.toVector
    }

    // collect event arguments as text bound mentions
    val entities: Seq[TextBoundMention] = (events ++ relations).flatMap { e =>
      val tbms = e.arguments.values.flatten.collect {
        case m: TextBoundMention => m
        case m: RelationMention => new TextBoundMention(m.labels, m.tokenInterval, m.sentence, m.document, m.keep, m.foundBy)
        case m: EventMention => m.trigger
      }
      tbms.toVector
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
      "geoexps" -> mkJsonFromLocationExpressions(location),

      "triggers" -> mkJsonFromEntities(triggers, tbMentionToId),
      "events" -> mkJsonFromEventMentions(events, tbMentionToId),
      "relations" -> mkJsonFromRelationMentions(relations, tbMentionToId)
    )
  }

  def mkJsonFromEntities(mentions: Seq[TextBoundMention], tbmToId: Map[TextBoundMention, Int]): Json.JsValueWrapper = {
    val entities = mentions.map(m => mkJsonFromTextBoundMention(m, tbmToId(m)))
    Json.arr(entities: _*)
  }

  def mkJsonFromTextBoundMention(m: TextBoundMention, i: Int): Json.JsValueWrapper = {
    Json.arr(
      s"T$i",
      this.statefulRepresentation(m).label,
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

  def mkJsonFromRelationMentions(rr: Seq[RelationMention], tbmToId: Map[TextBoundMention, Int]): Json.JsValueWrapper = {
    var i = 0
    val jsonRelations = for (r <- rr) yield {
      i += 1
      mkJsonFromRelationMention(r, i, tbmToId)
    }
    Json.arr(jsonRelations: _*)
  }

  // fixme: ordering/precedence...
  protected def statefulRepresentation(m: Mention): Mention = {
    val stateAffix = m.attachments match {
      case inc if inc.exists(a => a.isInstanceOf[Increase]) => INC_LABEL_AFFIX
      case dec if dec.exists(a => a.isInstanceOf[Decrease]) => DEC_LABEL_AFFIX
      case quant if quant.exists(a => a.isInstanceOf[Quantification]) => QUANT_LABEL_AFFIX
      case pos if pos.exists(a => a.isInstanceOf[PosChange]) => POS_LABEL_AFFIX
      case neg if neg.exists(a => a.isInstanceOf[NegChange]) => NEG_LABEL_AFFIX
      case _ => ""
    }

    // If you found something, append the affix to top label and add to the Seq of labels
    if (stateAffix.nonEmpty) {
      val modifiedLabels = Seq(m.label ++ stateAffix) ++ m.labels
      val out = m match {
        case tb: TextBoundMention => tb.copy(labels = modifiedLabels)
        case rm: RelationMention => rm.copy(labels = modifiedLabels)
        case em: EventMention => em.copy(labels = modifiedLabels)
      }

      return out
    }

    // otherwise, return original
    m
  }

  def getArg(r: RelationMention, name: String): TextBoundMention = r.arguments(name).head match {
    case m: TextBoundMention => m
    case m: EventMention => m.trigger
    case m: RelationMention => ???
  }

  def mkJsonFromRelationMention(r: RelationMention, i: Int, tbmToId: Map[TextBoundMention, Int]): Json.JsValueWrapper = {
    Json.arr(
      s"R$i",
      r.label,
      // arguments are hardcoded to ensure the direction (controller -> controlled)
      Json.arr(
        Json.arr("cause", "T" + tbmToId(getArg(r, "cause"))),
        Json.arr("effect", "T" + tbmToId(getArg(r, "effect")))
      )
    )
  }

  def mkJsonFromTimeExpressions(time: Option[Array[Seq[TimEx]]]): Json.JsValueWrapper = {
    val result = time.map { time =>
      var x = 0
      val timexs = for (t <- time; i <- t) yield {
        x += 1
        Json.arr(
          s"X$x",
          "TimeExpression",
          Json.arr(Json.arr(i.span.start, i.span.end)),
          Json.toJson(for (d <- i.intervals) yield (
              d.startDate.toString,
              d.endDate.toString))
        )
      }
      Json.toJson(timexs)
    }.getOrElse(Json.toJson(Json.arr()))
    result
  }

  def mkJsonFromLocationExpressions(location: Option[Array[Seq[GeoPhraseID]]]): Json.JsValueWrapper = {
    val result = location.map { location =>
      var x = 0
      val geoexps = for (t <- location; i <- t) yield {
        x += 1
        Json.arr(
          s"G$x",
          "GeoLocation",
          Json.arr(Json.arr(i.startOffset, i.endOffset)),
          Json.toJson(i.geonameID)
        )
      }
      Json.toJson(geoexps)
    }.getOrElse(Json.toJson(Json.arr()))
    result
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
        case m: CrossSentenceMention => m.anchor.asInstanceOf[TextBoundMention]
      }
      mkArgMention(argRole, s"T${tbmToId(arg)}")
    }
    args.toSeq
  }

  def mkArgMention(argRole: String, id: String): Json.JsValueWrapper = {
    Json.arr(argRole, id)
  }

  //object HomeController {
  val INC_LABEL_AFFIX = "-Inc"
  val DEC_LABEL_AFFIX = "-Dec"
  val POS_LABEL_AFFIX = "-Pos"
  val NEG_LABEL_AFFIX = "-Neg"
  val QUANT_LABEL_AFFIX = "-Quant"
}
