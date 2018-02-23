package org.clulab.wm.eidos.apps

// Code after "This code here" was removed from EidosShell and may be reincorporated later.


//package agro.demo
//
//import org.clulab.odin.Mention
//import org.clulab.processors.Sentence
//import org.clulab.wm.{EidosSystem, Decrease, Increase, Quantification}
//
//import utils.CliReader
//import utils.DisplayUtils.displayMentions
//import utils.IdeReader
//
//import scala.collection.immutable.ListMap
//
///**
//  * Interactive shell for parsing RAPs
//  */
//
//object EidosShell extends App {
//
////  import org.clulab.wm.Aliases._
//
//  //TODO: Load the parameters to the system through a config file
////  val config = ConfigFactory.load()         // load the configuration file
////  val quantifierKBFile: String = config[String]("wmseed.quantifierKB")
////  val domainParamKBFile: String = config[String]("wmseed.domainParamKB")
//
//  val reader = {
//    val prompt = "(Eidos)>>> "
//
//    if (args.length == 0) new CliReader(prompt, "user.home", ".eidosshellhistory")
//    else new IdeReader(prompt)
//  }
//
//  val commands = ListMap(
//    ":help" -> "show commands",
//    ":reload" -> "reload grammar",
//    ":exit" -> "exit system"
//  )
//
//  // creates an extractor engine using the rules and the default actions
//  val ieSystem = new EidosSystem()
//
//  //var proc = ieSystem.proc // Not used
//
//  println("\nWelcome to the Eidos Shell!")
//  printCommands()
//
//  var running = true
//
//  while (running) {
//    val line = reader.readLine
//    line match {
//      case ":help" =>
//        printCommands()
//
//      case ":reload" =>
//        ieSystem.reload()
//
//      case ":exit" | null =>
//        running = false
//
//      case text =>
//        extractFrom(text)
//    }
//  }
//
//  // summarize available commands
//  def printCommands(): Unit = {
//    println("\nCOMMANDS:")
//    for ((cmd, msg) <- commands)
//      println(s"\t$cmd\t=> $msg")
//    println()
//  }
//
//  def extractFrom(text:String): Unit = {
//
//    // preprocessing
//    val doc = ieSystem.annotate(text)
//
//    // extract mentions from annotated document
//    val mentions = ieSystem.extractFrom(doc).sortBy(m => (m.sentence, m.getClass.getSimpleName))
//
//    // debug display the mentions
//    displayMentions(mentions, doc, true)
//
//    // pretty display
////    prettyDisplay(mentions, doc)
//
//  }


// This code here!



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

/* This is not being used
  
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
  
*/
//}

//case class ParamInstance(justification: String, sentence: String, quantifiers: Option[Seq[String]], param: Seq[String])
//case class GroundedParamInstance(justification: String, sentence: String, quantifiers: Option[Seq[String]], groundedEntity: Option[String], predictedDelta: Option[Double], mean: Option[Double], stdev: Option[Double], gradableAdj: Option[String])
