package org.clulab.wm.eidos.apps
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.groundings.{OntologyGrounder, OntologyHandler}
import java.io.PrintWriter

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.struct.Interval
import org.clulab.wm.eidos.EidosSystem

object GenerateGoldGroundingTSV extends App {

  // load tsv files from resources
  val originalFile = FileUtils.getTextFromFile("compositionalGrounderTestDoc/groundingEvalEntities.tsv")
  val fileAsString: String = originalFile.toString
  val lines: Array[String] = fileAsString.split("\n")

  var outFilename = "compositionalGrounderTestDoc/gold_groundings.tsv"
  val header =
    "GOLD Annotated?\t" +
    "Index\t" +
    "Sentence\t" +
    "Entity\t" +
    "Character Offsets\t" +
    "GOLD Flat Grounding\t" +
    "GOLD Flat Grounding Score\t" +
    "GOLD Concept Grounding\t" +
    "GOLD Concept Score\t" +
    "GOLD Process Grounding\t" +
    "GOLD Process Score\t" +
    "GOLD Property Grounding\t" +
    "GOLD Property Score"

  val pw = new PrintWriter(outFilename)
  pw.println(header)

  val proc = new FastNLPProcessor()

  // Regex replacement patterns
  // Only needed if we try to de-tokenize
  val commaPattern = " ,"
  val commaPatternReplaced = ","
  val periodPattern = " \\."
  val periodPatternReplaced = "\\."
  val possessivePattern = " 's "
  val possessivePatternReplaced = "'s "
  val weirdPossessivePattern = " ' s "
  val weirdPossessivePatternReplaced = "'s "
  val apostrophePattern = " ' "
  val apostrophePatternReplaced = " "
  val LParenPattern = "\\( "
  val LParenPatternReplaced = "\\("
  val RParenPattern = " \\)"
  val RParenPatternReplaced = "\\)"
  val LBracketPattern = "\\[ "
  val LBracketPatternReplaced = "\\["
  val RBracketPattern = " \\]"
  val RBracketPatternReplaced = "\\]"
  val doubleHyphenPattern = " - - "
  val doubleHyphenPatternReplaced = "--"
  val hyphenPattern = "( ?)-( ?)"
  val hyphenPatternReplaced = "-"
  val $Pattern = "\\$ "
  val $PatternReplaced = "\\$"
  val funkyTokenPattern = "Ģ"
  val funkyTokenPatternReplaced = "*"
  val kgPattern = " kg"
  val kgPatternReplaced = "kg"

  val ontologyHandler: OntologyHandler = new
      EidosSystem().components.ontologyHandler
  val ieSystem = new EidosSystem()

  for (entry <- lines) {
    val line = entry.split("\t")

    val index = line(0)

    val sentence = line(18)
    // do all de-tokenizations here
    // FIXME: could probably done way, way better
//    sentence = sentence.replaceAll(commaPattern,commaPatternReplaced)
//    sentence = sentence.replaceAll(periodPattern,periodPatternReplaced)
//    sentence = sentence.replaceAll(possessivePattern,possessivePatternReplaced)
//    sentence = sentence.replaceAll(weirdPossessivePattern,weirdPossessivePatternReplaced)
//    sentence = sentence.replaceAll(apostrophePattern,apostrophePatternReplaced)
//    sentence = sentence.replaceAll(LParenPattern,LParenPatternReplaced)
//    sentence = sentence.replaceAll(RParenPattern,RParenPatternReplaced)
//    sentence = sentence.replaceAll(LBracketPattern,LBracketPatternReplaced)
//    sentence = sentence.replaceAll(RBracketPattern,RBracketPatternReplaced)
//    sentence = sentence.replaceAll(doubleHyphenPattern,doubleHyphenPatternReplaced)
//    sentence = sentence.replaceAll(hyphenPattern,hyphenPatternReplaced)
//    sentence = sentence.replaceAll($Pattern,$PatternReplaced)
//    sentence = sentence.replaceAll(funkyTokenPattern,funkyTokenPatternReplaced)
//    sentence = sentence.replaceAll(kgPattern,kgPatternReplaced)

    val cause = line(4)
    // do all de-tokenizations here
    // FIXME: could probably done way, way better
//    cause = cause.replaceAll(commaPattern,commaPatternReplaced)
//    cause = cause.replaceAll(periodPattern,periodPatternReplaced)
//    cause = cause.replaceAll(possessivePattern,possessivePatternReplaced)
//    cause = cause.replaceAll(weirdPossessivePattern,weirdPossessivePatternReplaced)
//    cause = cause.replaceAll(apostrophePattern,apostrophePatternReplaced)
//    cause = cause.replaceAll(LParenPattern,LParenPatternReplaced)
//    cause = cause.replaceAll(RParenPattern,RParenPatternReplaced)
//    cause = cause.replaceAll(LBracketPattern,LBracketPatternReplaced)
//    cause = cause.replaceAll(RBracketPattern,RBracketPatternReplaced)
//    cause = cause.replaceAll(doubleHyphenPattern,doubleHyphenPatternReplaced)
//    cause = cause.replaceAll(hyphenPattern,hyphenPatternReplaced)
//    cause = cause.replaceAll($Pattern,$PatternReplaced)
//    cause = cause.replaceAll(funkyTokenPattern,funkyTokenPatternReplaced)
//    cause = cause.replaceAll(kgPattern,kgPatternReplaced)

//    val cause = proc.annotate(line(4)).sentences.head.words.mkString(" ")
    val causeStartOffset = sentence indexOf cause
    val causeEndOffset = causeStartOffset+cause.length
    val causeOffset: Interval = Interval(causeStartOffset,causeEndOffset)


    val effect = line(11)
    // do all de-tokenizations here
    // FIXME: could probably done way, way better
//    effect = effect.replaceAll(commaPattern,commaPatternReplaced)
//    effect = effect.replaceAll(periodPattern,periodPatternReplaced)
//    effect = effect.replaceAll(possessivePattern,possessivePatternReplaced)
//    effect = effect.replaceAll(weirdPossessivePattern,weirdPossessivePatternReplaced)
//    effect = effect.replaceAll(apostrophePattern,apostrophePatternReplaced)
//    effect = effect.replaceAll(LParenPattern,LParenPatternReplaced)
//    effect = effect.replaceAll(RParenPattern,RParenPatternReplaced)
//    effect = effect.replaceAll(LBracketPattern,LBracketPatternReplaced)
//    effect = effect.replaceAll(RBracketPattern,RBracketPatternReplaced)
//    effect = effect.replaceAll(doubleHyphenPattern,doubleHyphenPatternReplaced)
//    effect = effect.replaceAll(hyphenPattern,hyphenPatternReplaced)
//    effect = effect.replaceAll($Pattern,$PatternReplaced)
//    effect = effect.replaceAll(funkyTokenPattern,funkyTokenPatternReplaced)
//    effect = effect.replaceAll(kgPattern,kgPatternReplaced)

//    val effect = proc.annotate(line(11)).sentences.head.words.mkString(" ")
    val effectStartOffset = sentence indexOf effect
    val effectEndOffset = effectStartOffset+effect.length
    val effectOffset: Interval = Interval(effectStartOffset,effectEndOffset)

    val document = ieSystem.annotate(sentence)

//    println("Index:\t"+ index)
//    println("Sentence:\t"+sentence)
//    println("Length:\t"+sentence.length())

    if (causeStartOffset != -1) {
//      println("Length:\t"+sentence.length())
//      println("causeOffset:\t"+causeOffset)
      val allGroundings = ontologyHandler.reground(sentence, causeOffset, document)

      val flatGroundings = allGroundings("wm_flattened")
      val flatName = flatGroundings.headOption.get._1.name
      val flatScore = flatGroundings.headOption.get._2

      val conceptGroundings = allGroundings("wm_compositional/concept")
      val conceptName = if (conceptGroundings.headOption.isDefined) conceptGroundings.headOption.get._1.name else None
      val conceptScore = if (conceptGroundings.headOption.isDefined) conceptGroundings.headOption.get._2 else None

      val processGroundings = allGroundings("wm_compositional/process")
      val processName = if (processGroundings.headOption.isDefined) processGroundings.headOption.get._1.name else None
      val processScore = if (processGroundings.headOption.isDefined) processGroundings.headOption.get._2 else None

      val propertyGroundings = allGroundings("wm_compositional/property")
      val propertyName = if (propertyGroundings.headOption.isDefined) propertyGroundings.headOption.get._1.name else None

      val propertyScore = if (propertyGroundings.headOption.isDefined) propertyGroundings.headOption.get._2 else None

      //    println("FLAT:")
      //    println(flatGroundings.headOption.get._1)
      //
      //    println("\nCONCEPT:")
      //    println(conceptGroundings.headOption.get._1)
      //
      //    println("\nPROCESS:")
      //    println(processGroundings.headOption.get._1)
      //
      //    println("\nPROPERTY:")
      //    println(propertyGroundings.headOption.get._1)

      val row1 =
        "" + "\t" +
        index + "\t" +
        sentence.trim() + "\t" +
        cause + "\t" +
        causeOffset + "\t" +
        flatName + "\t" +
        flatScore + "\t" +
        conceptName + "\t" +
        conceptScore + "\t" +
        processName + "\t" +
        processScore + "\t" +
        propertyName + "\t" +
        propertyScore + "\n"

      pw.print(row1)
    }

    if (effectStartOffset != -1) {
//      println("Length:\t"+sentence.length())
//      println("effectOffset:\t"+effectOffset)

      val allGroundings = ontologyHandler.reground(sentence, effectOffset, document)

      val flatGroundings = allGroundings("wm_flattened")
      val flatName = if (flatGroundings.headOption.isDefined) flatGroundings.headOption.get._1.name else None
      val flatScore = if(flatGroundings.headOption.isDefined) flatGroundings.headOption.get._2 else None

      val conceptGroundings = allGroundings("wm_compositional/concept")
      val conceptName = if (conceptGroundings.headOption.isDefined) conceptGroundings.headOption.get._1.name else None
      val conceptScore = if (conceptGroundings.headOption.isDefined) conceptGroundings.headOption.get._2 else None

      val processGroundings = allGroundings("wm_compositional/process")
      val processName = if (processGroundings.headOption.isDefined) processGroundings.headOption.get._1.name else None
      val processScore = if (processGroundings.headOption.isDefined) processGroundings.headOption.get._2 else None

      val propertyGroundings = allGroundings("wm_compositional/property")
      val propertyName = if (propertyGroundings.headOption.isDefined) propertyGroundings.headOption.get._1.name else None

      val propertyScore = if (propertyGroundings.headOption.isDefined) propertyGroundings.headOption.get._2 else None

      //    println("FLAT:")
      //    println(flatGroundings.headOption.get._1)
      //
      //    println("\nCONCEPT:")
      //    println(conceptGroundings.headOption.get._1)
      //
      //    println("\nPROCESS:")
      //    println(processGroundings.headOption.get._1)
      //
      //    println("\nPROPERTY:")
      //    println(propertyGroundings.headOption.get._1)

      val row2 =
        "" + "\t" +
        index + "\t" +
        sentence.trim() + "\t" +
        effect + "\t" +
        effectOffset + "\t" +
        flatName + "\t" +
        flatScore + "\t" +
        conceptName + "\t" +
        conceptScore + "\t" +
        processName + "\t" +
        processScore + "\t" +
        propertyName + "\t" +
        propertyScore + "\n"

      pw.print(row2)
    }
  }
  pw.close()
}
