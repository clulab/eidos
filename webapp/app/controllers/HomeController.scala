package controllers

import com.typesafe.config.Config
import javax.inject._
import com.typesafe.config.ConfigRenderOptions
import org.clulab.odin._
import org.clulab.processors.{Document, Sentence}
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.BuildInfo
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.Aliases._
import org.clulab.wm.eidos.context.GeoNormFinder
import org.clulab.wm.eidos.context.GeoPhraseID
import org.clulab.wm.eidos.context.TimEx
import org.clulab.wm.eidos.context.TimeNormFinder
import org.clulab.wm.eidos.groundings.EidosOntologyGrounder
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.{DisplayUtils, DomainParams, GroundingUtils, MaaSUtils, PlayUtils}
import play.api.mvc._
import play.api.libs.json._
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.groundings.EidosAdjectiveGrounder
import play.api.mvc.Action

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  // Initialize the EidosSystem
  // -------------------------------------------------
  println("[EidosSystem] Initializing the EidosSystem ...")
  val eidosConfig: Config = EidosSystem.defaultConfig
  val ieSystem: EidosSystem = new EidosSystem(eidosConfig)
  val stanza = "adjectiveGrounder"
  val adjectiveGrounder: EidosAdjectiveGrounder = EidosAdjectiveGrounder.fromConfig(eidosConfig.getConfig(stanza))
  val domainParams: DomainParams = DomainParams.fromConfig(eidosConfig.getConfig(stanza))
  println("[EidosSystem] Completed Initialization ...")

  {
    println("[EidosSystem] Priming the EidosSystem ...")
    val annotatedDocument = 
        ieSystem.extractFromText("In 2014 drought caused a famine in Ethopia.", cagRelevantOnly = true, Some("2019-08-09"))
    val corpus = new JLDCorpus(annotatedDocument)
    val mentionsJSONLD = corpus.serialize(adjectiveGrounder)
    println("[EidosSystem] Completed Priming ...")
  }
  // -------------------------------------------------

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def buildInfo: Action[AnyContent] = Action {
    Ok(jsonBuildInfo)
  }

  def config: Action[AnyContent] = Action {
    val options: ConfigRenderOptions = ConfigRenderOptions.concise.setFormatted(true).setJson(true)
    val jsonString = eidosConfig.root.render(options)
    Ok(jsonString).as(JSON)
  }

  // -------------------------------------------
  //      API entry points for MaaS
  // -------------------------------------------

  def mapNode: Action[AnyContent] = Action { request =>
    val data = request.body.asJson.get.toString()
    // Note -- topN can be exposed to the API if needed
    Ok(MaaSUtils.mapNodeToPrimaryConcepts(ieSystem, data, topN = 10)).as(JSON)
  }

  def mapOntology: Action[AnyContent] = Action { request =>
    val fileContents = request.body.asText.get
    // Note -- topN can be exposed to the API if needed
    Ok(MaaSUtils.mapOntology(ieSystem, "MaaS", fileContents, topN = 10)).as(JSON)
  }


  // Entry method
  def parseText(text: String, cagRelevantOnly: Boolean): Action[AnyContent] = Action {
    val (doc, eidosMentions, groundedEntities) = processPlaySentence(ieSystem, text, cagRelevantOnly)
    println(s"Sentence returned from processPlaySentence : ${doc.sentences.head.getSentenceText}")
    val json = mkJson(text, doc, eidosMentions, groundedEntities)
    Ok(json)
  }

  // Method where eidos happens!
  def processPlaySentence(
    ieSystem: EidosSystem,
    text: String,
    cagRelevantOnly: Boolean): (Document, Vector[EidosMention], Vector[GroundedEntity]) = {

    def mentionOrder(m: Mention): Int = 10000 * m.sentence + m.start

    // preprocessing
    println(s"Processing sentence : $text" )
    val doc = ieSystem.annotate(text)

    // Debug
//    println(s"DOC : $doc")
    // extract mentions from annotated document
    val annotatedDocument = ieSystem.extractFromText(text, cagRelevantOnly = cagRelevantOnly)
    val mentions = annotatedDocument.eidosMentions.sortBy(m => (m.odinMention.sentence, m.getClass.getSimpleName)).toVector


    println(s"Done extracting the mentions ... ")
    println(s"They are : ${mentions.map(m => m.odinMention.text).mkString(",\t")}")

    println(s"Grounding the gradable adjectives ... ")
    val groundedEntities = groundEntities(ieSystem, mentions)

    println("DONE .... ")
    //    println(s"Grounded Adjectives : ${groundedAdjectives.size}")
    // return the sentence and all the mentions extracted ... TODO: fix it to process all the sentences in the doc
    (doc, mentions.sortBy(m => mentionOrder(m.odinMention)), groundedEntities)
  }

// Webservice functions
  def process_text: Action[JsValue] = Action(parse.json) { request =>
    (request.body \ "text").asOpt[String].map { text =>
      val mentionsJSONLD = processPlaytext(ieSystem, text)
      val parsed_output = PlayUtils.toPlayJson(mentionsJSONLD)
      Ok(parsed_output)
    }.getOrElse {
      BadRequest("Missing parameter [text]")
    }
  }

  def reground: Action[JsValue] = Action(parse.json) { request =>

    def extract(name: String): JsLookupResult = request.body \ name

    try {
      val name = extract("name").asOpt[String].getOrElse("Custom")
      val ontologyYaml = extract("ontologyYaml").as[String]
      val texts = extract("texts").as[JsArray].value.map { jsString =>
        (jsString: @unchecked) match {
          case JsString(text) => text
        }
      }
      val filter = extract("filter").asOpt[Boolean].getOrElse(true)
      val topk = extract("topk").asOpt[Int].getOrElse(10)
      val isAlreadyCanonicalized = extract("isAlreadyCanonicalized").asOpt[Boolean].getOrElse(true)

      try {
        val ontologyHandler = ieSystem.components.ontologyHandler
        val regroundings = ontologyHandler.reground(name, ontologyYaml, texts, filter, topk, isAlreadyCanonicalized)
        val result = JsArray { regroundings.map { regrounding =>
            JsArray { regrounding.map { case (grounding, score) =>
                JsObject(Map(
                  "grounding" -> JsString(grounding),
                  "score" -> JsNumber(score.toDouble)
                ))
            }}
        }}

        Ok(result)
      }
      catch {
        case throwable: Throwable =>
          InternalServerError(JsString(s"The server couldn't handle it: ${throwable.getMessage}"))
      }
    }
    catch {
      case throwable: Throwable =>
        BadRequest(JsString(s"The request seems to be bad: ${throwable.getMessage}"))
    }
  }

  // Method where eidos processing for webservice happens
  def processPlaytext(
    ieSystem: EidosSystem,
    text: String): org.json4s.JsonAST.JValue = {

    // preprocessing
    println(s"Processing sentence : $text" )
    val annotatedDocument = ieSystem.extractFromText(text)

    // Export to JSON-LD
    val corpus = new JLDCorpus(annotatedDocument)
    val mentionsJSONLD = corpus.serialize(adjectiveGrounder)
    mentionsJSONLD
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
    println("loaded domain params:" + domainParams.toString())
    println(s"\tkeys: ${domainParams.keys.mkString(", ")}")
    println(s"getting details for: ${mention.text}")

    val paramDetails: Map[String, Double] = domainParams.get(DomainParams.DEFAULT_DOMAIN_PARAM).get
    val paramMean = paramDetails(DomainParams.PARAM_MEAN)
    val paramStdev = paramDetails(DomainParams.PARAM_STDEV)
    val grounding = adjectiveGrounder.groundAdjective(quantifier)
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
    "gitUncommittedChanges" -> BuildInfo.gitUncommittedChanges /* ,
    // These values change with each compilation and force repackaging.
    // Since they are not being used at all anyway, they are no longer included.
    // See build.sbt where a related line is commented out.
    "builtAtString" -> BuildInfo.builtAtString,
    "builtAtMillis" -> BuildInfo.builtAtMillis */
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

  def mkJson(text: String, doc: Document, eidosMentions: Vector[EidosMention], groundedEntities: Vector[GroundedEntity] ): JsValue = {
    println("Found mentions (in mkJson):")
    eidosMentions.foreach(eidosMention => DisplayUtils.displayMention(eidosMention.odinMention))

    val odinMentions = eidosMentions.map(_.odinMention)
    val timExs = ieSystem.components.timeNormFinderOpt.map(_.getTimExs(odinMentions, doc.sentences))
    val geoPhraseIDs = ieSystem.components.geoNormFinderOpt.map(_.getGeoPhraseIDs(odinMentions, doc.sentences))
    val sent = doc.sentences.head
    val syntaxJsonObj = Json.obj(
        "text" -> text,
        "entities" -> mkJsonFromTokens(doc),
        "relations" -> mkJsonFromDependencies(doc)
      )
    val eidosJsonObj = mkJsonForEidos(text, sent, odinMentions, timExs, geoPhraseIDs)
    val groundedAdjObj = mkGroundedObj(groundedEntities, eidosMentions, timExs, geoPhraseIDs)
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

  def mkGroundedObj(groundedEntities: Vector[GroundedEntity], mentions: Vector[EidosMention], time: Option[Array[Seq[TimEx]]], location: Option[Array[Seq[GeoPhraseID]]]): String = {

    var objectToReturn = ""

    if (groundedEntities.nonEmpty) {
      objectToReturn += "<h2>Grounded Concepts:</h2>"

      // Make the string for each grounded entity
      val toPrint = for (grounding <- groundedEntities) yield {
        val sentence = grounding.sentence
        val quantifier = grounding.quantifier
        val groundedEntity = grounding.entity
        val predictedDelta = grounding.predictedDelta
        val mean = grounding.mean
        val stdev = grounding.stdev
        var stringToYield = s"${tab}Sentence: $sentence"

        stringToYield += s"<br>${tab}Entity: $groundedEntity"
        stringToYield += s"<br>${tab}Quantifier: $quantifier"
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
    val timeMentions = TimeNormFinder.getTimExs(mentions.map(_.odinMention))
    if (timeMentions.nonEmpty) {
      objectToReturn += "<h2>Found TimeExpressions:</h2>"
      objectToReturn += s"${DisplayUtils.webAppTimeExpressions(timeMentions)}"
    }

    // GeoLocations
    val locationMentions = GeoNormFinder.getGeoPhraseIDs(mentions.map(_.odinMention))
    if (locationMentions.nonEmpty) {
      objectToReturn += "<h2>Found GeoLocations:</h2>"
      objectToReturn += s"${DisplayUtils.webAppGeoLocations(locationMentions)}"
    }

    // Concepts
    val entities = mentions.filter(_.odinMention matches "Entity")
    if (entities.nonEmpty){
      objectToReturn += "<h2>Found Concepts:</h2>"
      for (entity <- entities) {
        objectToReturn += s"${DisplayUtils.webAppMention(entity.odinMention)}"
        // If the primary groundings are available, let's print them too...
        val groundingStringOpt = GroundingUtils.getGroundingsStringOpt(entity, EidosOntologyGrounder.PRIMARY_NAMESPACE, 5, s"<br>${DisplayUtils.htmlTab}${DisplayUtils.htmlTab}")
        if (groundingStringOpt.isDefined) {
          objectToReturn += s"${DisplayUtils.htmlTab}OntologyLinkings:<br>${DisplayUtils.htmlTab}${DisplayUtils.htmlTab}"
          objectToReturn +=  groundingStringOpt
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

  def mkJsonForEidos(sentenceText: String, sent: Sentence, mentions: Vector[Mention], time: Option[Array[Seq[TimEx]]], location: Option[Array[Seq[GeoPhraseID]]]): Json.JsValueWrapper = {
    val topLevelTBM = mentions.collect { case m: TextBoundMention => m }

    // collect event mentions for display
    val events = mentions.collect { case m: EventMention => m }

    // collect relation mentions for display
    val relations = mentions.collect { case m: RelationMention => m }

    // collect triggers for event mentions
    val triggers: Vector[TextBoundMention] = events.flatMap { e =>
      val argTriggers = e.arguments.values.flatten.collect { case m: EventMention => m.trigger }
      e.trigger +: argTriggers.toVector
    }

    // collect event arguments as text bound mentions
    val entities: Vector[TextBoundMention] = (events ++ relations).flatMap { e =>
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

  def mkJsonFromRelationMentions(rr: Seq[RelationMention], tbmToId: Map[TextBoundMention, Int]): Json.JsValueWrapper = {
    var i = 0
    val jsonRelations = for (r <- rr) yield {
      i += 1
      mkJsonFromRelationMention(r, i, tbmToId)
    }
    Json.arr(jsonRelations: _*)
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

  def mkJsonFromTokens(doc: Document): Json.JsValueWrapper = {
    var offset = 0

    val tokens = doc.sentences.flatMap { sent =>
      val tokens = sent.words.indices.map(i => mkJsonFromToken(sent, offset, i))
      offset += sent.words.length
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
      offset += sent.words.length
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

  def tab:String = "&nbsp;&nbsp;&nbsp;&nbsp;"
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
        case tb: TextBoundMention => tb.copy(labels = modifiedLabels)
        case rm: RelationMention => rm.copy(labels = modifiedLabels)
        case em: EventMention => em.copy(labels = modifiedLabels)
      }

      return out
    }

    // otherwise, return original
    m
  }

}
