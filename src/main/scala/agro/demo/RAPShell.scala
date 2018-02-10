package agro.demo

import java.io.File

import org.clulab.odin.{Attachment, EventMention, Mention}
import org.clulab.processors.{Document, Sentence}
import org.clulab.sequences.LexiconNER
import org.clulab.wm.{OpenIESystem, Decrease, Increase, Quantification}
import utils.DisplayUtils.displayMentions

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/**
  * Line reader to use in different environments.  The CliReader works on the command line
  * and with sbt and supports history.  However, it doesn't work well in the IntelliJ
  * or Eclipse IDEs (as least not with Windows).  For those, use the IdeReader.  To switch
  * between the two, add a command line argument to get the IdeReader rather than changing
  * the code.
  */

abstract class LineReader {
  def readLine(): String
}

class CliReader(prompt: String, parentProperty: String, child: String) extends LineReader {
  import jline.console.ConsoleReader
  import jline.console.history.FileHistory

  val reader = new ConsoleReader()
  val history = new FileHistory(new File(System.getProperty(parentProperty), child))

  reader.setPrompt(prompt)
  reader.setHistory(history)
  sys addShutdownHook {
    reader.getTerminal.restore()
    reader.shutdown()
    history.flush() // flush file before exiting
  }

  override def readLine = reader.readLine
}

class IdeReader(protected var prompt: String) extends LineReader {
  import java.util.Scanner

  protected val reader = new Scanner(System.in)

  override def readLine = {
    print(prompt)
    Console.flush()
    reader.nextLine
  }
}

/**
  * Interactive shell for parsing RAPs
  */

object RAPShell extends App {

  import org.clulab.wm.Aliases._

  //TODO: Load the parameters to the system through a config file
//  val config = ConfigFactory.load()         // load the configuration file
//  val quantifierKBFile: String = config[String]("wmseed.quantifierKB")
//  val domainParamKBFile: String = config[String]("wmseed.domainParamKB")

  val reader = {
    val prompt = "(RAP)>>> "

    if (args.length == 0) new CliReader(prompt, "user.home", ".agroshellhistory")
    else new IdeReader(prompt)
  }

  val commands = ListMap(
    ":help" -> "show commands",
    ":reload" -> "reload grammar",
    ":exit" -> "exit system"
  )

  // creates an extractor engine using the rules and the default actions
  val ieSystem = new OpenIESystem()

  //var proc = ieSystem.proc // Not used

  println("\nWelcome to the RAPShell!")
  printCommands()

  var running = true

  while (running) {
    val line = reader.readLine
    line match {
      case ":help" =>
        printCommands()

      case ":reload" =>
        ieSystem.reload()

      case ":exit" | null =>
        running = false

      case text =>
        extractFrom(text)
    }
  }

  // summarize available commands
  def printCommands(): Unit = {
    println("\nCOMMANDS:")
    for ((cmd, msg) <- commands)
      println(s"\t$cmd\t=> $msg")
    println()
  }

  def extractFrom(text:String): Unit = {

    // preprocessing
    val doc = ieSystem.annotate(text)

    // extract mentions from annotated document
    val mentions = ieSystem.extractFrom(doc).sortBy(m => (m.sentence, m.getClass.getSimpleName))

    // debug display the mentions
    displayMentions(mentions, doc, true)

    // pretty display
//    prettyDisplay(mentions, doc)

  }

  def processPlaySentence(ieSystem: OpenIESystem, text: String): (Sentence, Vector[Mention], Vector[GroundedEntity], Vector[(Trigger, Map[String, String])]) = {
    // preprocessing
    println(s"Processing sentence : ${text}" )
    val doc = ieSystem.annotate(text)

    println(s"DOC : ${doc}")
    // extract mentions from annotated document
    val mentions = ieSystem.extractFrom(doc).sortBy(m => (m.sentence, m.getClass.getSimpleName))
    println(s"Done extracting the mentions ... ")

    println(s"Grounding the gradable adjectives ... ")
    val groundedEntities = groundEntities(ieSystem, mentions)

    println(s"Getting entity linking events ... ")
    val events = getEntityLinkerEvents(mentions)

    println("DONE .... ")
//    println(s"Grounded Adjectives : ${groundedAdjectives.size}")
    // return the sentence and all the mentions extracted ... TODO: fix it to process all the sentences in the doc
    (doc.sentences.head, mentions.sortBy(_.start), groundedEntities, events)
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

  def convertToAdjective(adverb: String): String = {
    if (adverb.endsWith("ily")) {
      adverb.slice(0, adverb.length - 3) ++ "y"
    }

    adverb.slice(0, adverb.length - 2)
  }

  def groundEntity(mention: Mention, quantifier: Quantifier, ieSystem: OpenIESystem): GroundedEntity = {
    val pseudoStemmed = if (quantifier.endsWith("ly")) convertToAdjective(quantifier) else quantifier
    val modelRow = ieSystem.grounder.getOrElse(pseudoStemmed, Map.empty[String, Double])
    val intercept = modelRow.get(OpenIESystem.INTERCEPT)
    val mu = modelRow.get(OpenIESystem.MU_COEFF)
    val sigma = modelRow.get(OpenIESystem.SIGMA_COEFF)

    // add the calculation
    println("loaded domain params:" + ieSystem.domainParamValues.toString())
    println(s"\tkeys: ${ieSystem.domainParamValues.keys.mkString(", ")}")
    println(s"getting details for: ${mention.text}")

    val paramDetails = ieSystem.domainParamValues.get("DEFAULT").get
    val paramMean = paramDetails.get(OpenIESystem.PARAM_MEAN)
    val paramStdev = paramDetails.get(OpenIESystem.PARAM_STDEV)

    val predictedDelta = if (modelRow.nonEmpty && paramDetails.nonEmpty){
      Some(math.pow(math.E, intercept.get + (mu.get * paramMean.get) + (sigma.get * paramStdev.get)) * paramStdev.get)
    } else None

    GroundedEntity(mention.document.sentences(mention.sentence).getSentenceText(), quantifier, mention.text, predictedDelta, mu, sigma)
  }


  def groundEntities(ieSystem: OpenIESystem, mentions: Seq[Mention]): Vector[GroundedEntity] = {
    val gms = for {
      m <- mentions
      (quantifications, increases, decreases) = separateAttachments(m)

      groundedQuantifications = for {
        q <- quantifications
        quantTrigger = q.asInstanceOf[Quantification].quantifier
      } yield groundEntity(m, quantTrigger, ieSystem)

      groundedIncreases = for {
        inc <- increases
        quantTrigger <- inc.asInstanceOf[Increase].quantifier.getOrElse(Seq.empty[Quantifier])
      } yield groundEntity(m, quantTrigger, ieSystem)

      groundedDecreases = for {
        dec <- decreases
        quantTrigger <- dec.asInstanceOf[Decrease].quantifier.getOrElse(Seq.empty[Quantifier])
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

  // todo: if you want this functionality you need to actually reimplement it
//    (groundedParams, new Vector[(String, Map[String, String])] )

//    for (e <- events) {
//      val f = formalWithoutColor(e)
//      if (f.isDefined) {
//        val just = e.text
//        val sent = e.sentenceObj.getSentenceText
//        val quantifiers = e.arguments.get("quantifier") match {
//          case Some(quantifierMentions) => Some(quantifierMentions.map(_.text))
//          case None => None
//        }
//
//        val themes = e.arguments.getOrElse("theme", Seq[Mention]())
//        val themeTexts = themes.map(m => m.text)
//        val baseParamTexts = themes.flatMap(m => m.arguments.get("baseParam")).flatten.map(n => n.text)
//        val sortedParamTexts = (themeTexts ++ baseParamTexts).sortBy(-_.length)
//        params.getOrElseUpdate(f.get, new ListBuffer[ParamInstance]) += new ParamInstance(just, sent, quantifiers, sortedParamTexts)
//      }
//    }
//
//    if (params.nonEmpty) {
//      Console.out.println("RAP Parameters:")
//      // k is the display string (i.e., INCREASE in Productivity)
//      for (k <- params.keySet) {
//        val evidence = params.get(k).get
////        Console.out.println(s"$k : ${evidence.size} instances:")
//        for (ev <- evidence) {
////          Console.out.println(s"\tJustification: [${ev.justification}]")
////          Console.out.println(s"\tSentence: ${ev.sentence}")
//
//          // If there is a gradable adjective:
//          if (ev.quantifiers.isDefined) {
//            val eventQuantifiers = ev.quantifiers.get
////            Console.out.println(s"\tQuantifier: ${Console.MAGENTA} ${eventQuantifiers.mkString(", ")} ${Console.RESET}")
//
//            // Lookup mean and stdev of Param
//            val longestDatabaseMatch = ev.param.indexWhere(text => ieSystem.domainParamValues.contains(text))
//            val longestMatchText = if (longestDatabaseMatch != -1) ev.param(longestDatabaseMatch) else "DEFAULT"
//            val paramDetails = if (longestDatabaseMatch != -1) {
//              ieSystem.domainParamValues.get(longestMatchText)
//            } else {
//              ieSystem.domainParamValues.get(AgroSystem.DEFAULT_DOMAIN_PARAM)
//            }
//            if (paramDetails.isEmpty) throw new RuntimeException("Requested param not in database, and default is not working.")
//
//
//            // todo: Try head of Base string of Param as another backoff (?)
//
//            // Lookup the model row for the quantifier
//            // todo: only using head, is this ok?
//            val modelRow = grounder.get(ev.quantifiers.get.head)
//            if (modelRow.isDefined) {
//
//              val intercept = modelRow.get(AgroSystem.INTERCEPT)
//              val mu = modelRow.get(AgroSystem.MU_COEFF)
//              val sigma = modelRow.get(AgroSystem.SIGMA_COEFF)
//
//              // add the calculation
//              // TODO: incorporate backoff!!
//              val paramMean = paramDetails.get(AgroSystem.PARAM_MEAN)
//              val paramStdev = paramDetails.get(AgroSystem.PARAM_STDEV)
//
//              val predictedDelta = math.pow(math.E, intercept + (mu * paramMean) + (sigma * paramStdev)) * paramStdev
//
//              // display
////              Console.out.println(s"Predicted delta = ${Console.BOLD} ${"%3.3f".format(predictedDelta)} ${Console.RESET}  (base param $longestMatchText " +
////                s"[with typical mean=$paramMean and stdev=$paramStdev], gradable adj: ${eventQuantifiers.head})")
//              groundedParams.getOrElseUpdate(k, new ListBuffer[GroundedParamInstance]) += new GroundedParamInstance(ev.justification, ev.sentence, ev.quantifiers, Some(longestMatchText), Some(predictedDelta), Some(paramMean), Some(paramStdev), Some(eventQuantifiers.head))
//
//            }
//            else {
//              groundedParams.getOrElseUpdate(k, new ListBuffer[GroundedParamInstance]) += new GroundedParamInstance(ev.justification, ev.sentence, ev.quantifiers, Some(longestMatchText), None, None, None, Some(eventQuantifiers.head))
//            }
//
//          }
//          else{
//            groundedParams.getOrElseUpdate(k, new ListBuffer[GroundedParamInstance]) += new GroundedParamInstance(ev.justification, ev.sentence, None, None, None, None, None, None)
//          }
//        }
//      }


//  }

//  def prettyDisplay(mentions: Seq[Mention], doc: Document): Unit = {
//    val events = mentions.filter(_ matches "Event")
//    val params = new mutable.HashMap[String, ListBuffer[ParamInstance]]()
//    for (e <- events) {
//      val f = formal(e)
//      if (f.isDefined) {
//        val just = e.text
//        val sent = e.sentenceObj.getSentenceText
//        val quantifiers = e.arguments.get("quantifier") match {
//          case Some(quantifierMentions) => Some(quantifierMentions.map(_.text))
//          case None => None
//        }
//
//        val themes = e.arguments.getOrElse("theme", Seq[Mention]())
//        val themeTexts = themes.map(m => m.text)
//        val baseParamTexts = themes.flatMap(m => m.arguments.get("baseParam")).flatten.map(n => n.text)
//        val sortedParamTexts = (themeTexts ++ baseParamTexts).sortBy(-_.length)
//        params.getOrElseUpdate(f.get, new ListBuffer[ParamInstance]) += new ParamInstance(just, sent, quantifiers, sortedParamTexts)
//      }
//    }
//
//    if (params.nonEmpty) {
//      Console.out.println("RAP Parameters:")
//      // k is the display string (i.e., INCREASE in Productivity)
//      for (k <- params.keySet) {
//        val evidence = params.get(k).get
//        Console.out.println(s"$k : ${evidence.size} instances:")
//        for (ev <- evidence) {
//          Console.out.println(s"\tJustification: [${ev.justification}]")
//          Console.out.println(s"\tSentence: ${ev.sentence}")
//
//          // If there is a gradable adjective:
//          if (ev.quantifiers.isDefined) {
//            val eventQuantifiers = ev.quantifiers.get
//            Console.out.println(s"\tQuantifier: ${Console.MAGENTA} ${eventQuantifiers.mkString(", ")} ${Console.RESET}")
//          }
//
//        }
//      }
//      println()
//    }
//
//
//    // print the No_Change_Event and cause/effect events here
//
//    for (e <- events) {
//
//      if (e matches "No_Change_Event") {
//        // TODO: Complete this...
//      }
//      else if (e matches "Cause_and_Effect") {
//        println("Causal Event")
//        println("--------------------------------")
//        e match {
//          case em: EventMention =>
//            val trigger = em.trigger.text
//
//            val arguments = em.arguments.map { a =>
//              val name = a._1
//              val arg_mentions = a._2.map(_.text).mkString(" ")
//              (name, arg_mentions)
//            }
//
//            println(s"${Console.UNDERLINED} ${e.sentenceObj.getSentenceText} ${Console.RESET}")
//            println(s"\tTrigger: ${Console.BOLD} ${trigger} ${Console.RESET}")
//            println(s"\tArguments:")
//            arguments foreach { a =>
//              println(s"${Console.BOLD}\t  ${a._1} ${Console.RESET} => ${Console.BLUE_B} ${Console.BOLD} ${a._2} ${Console.RESET}")
//            }
//        }
//      }
//      println()
//    }
//  }


  // Returns Some(string) if there is an INCREASE or DECREASE event with a Param, otherwise None
  def formal(e:Mention):Option[String] = {
    var t = ""
    if(e matches "Decrease") t = s"${Console.RED} DECREASE ${Console.RESET}"
    else if(e matches "Increase") t = s"${Console.GREEN} INCREASE ${Console.RESET}"
    else return None

    Some(s"$t of ${Console.BLUE} ${e.arguments.get("theme").get.head.label} ${Console.RESET}")
  }

  // Returns Some(string) if there is an INCREASE or DECREASE event with a Param, otherwise None
  def formalWithoutColor(e:Mention):Option[String] = {
    var t = ""
    if(e matches "Decrease") t = s"DECREASE"
    else if(e matches "Increase") t = s"INCREASE"
    else return None

    Some(s"$t of ${e.arguments.get("theme").get.head.label}")
  }


}

//case class ParamInstance(justification: String, sentence: String, quantifiers: Option[Seq[String]], param: Seq[String])
case class GroundedParamInstance(justification: String, sentence: String, quantifiers: Option[Seq[String]], groundedEntity: Option[String], predictedDelta: Option[Double], mean: Option[Double], stdev: Option[Double], gradableAdj: Option[String])
