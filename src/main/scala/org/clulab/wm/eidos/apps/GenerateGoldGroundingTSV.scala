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
    "Index\t" +
    "Sentence\t" +
    "Entity\t" +
    "Character Offsets\t" +
    "GOLD Flat Grounding\t" +
    "GOLD Compositional Grounding"

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

  for (entry <- lines.take(1)) {
    val line = entry.split("\t")

    val index = line(0)

//    val sentence = line(18)
//    val sentence = proc.annotate(line(18)).sentences.head.words.mkString(" ")
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
//    val causeOffset = if (causeStartOffset != -1) (causeStartOffset,causeEndOffset) else "BAD OFFSETS!"
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
    val effectEndOffset = effectStartOffset+cause.length
//    val effectOffset = if (effectStartOffset != -1) (effectStartOffset,effectEndOffset) else "BAD OFFSETS!"
    val effectOffset: Interval = Interval(effectStartOffset,effectEndOffset)

    val ontologyHandler: OntologyHandler = new
        EidosSystem().components.ontologyHandler

//    val compOntologyGrounder: OntologyGrounder = ontologyHandler.ontologyGrounders.find(_.name == "wm_compositional").get

//    val flatGrounding1 = if (causeStartOffset != 1) ontologyHandler.reground(cause,causeOffset) else None
    val flatGrounding1 = ontologyHandler.reground(sentence,causeOffset)
    val compositionalGrounding1 = "TBD"
    println("FLAT:")
    println(flatGrounding1)
    println(flatGrounding1.head)
    println(flatGrounding1.head._2)
    println(flatGrounding1.head._2.grounding)
    println(flatGrounding1.head._2.grounding.head)
    
    println("\nCOMPOSITIONAL:")
    val compGrounding = flatGrounding1.tail
//    val propertyGrounding = flatGrounding1.tail("wm_compositional/property").grounding
//    val processGrounding = flatGrounding1.tail("wm_compositional/process").grounding.head
//    val conceptGrounding = flatGrounding1.tail("wm_compositional/concept")

    println(compGrounding)
//    println(propertyGrounding)
//    println(processGrounding)
//    println(conceptGrounding)

    val row1 =
      index + "\t" +
        sentence.trim() + "\t" +
        cause + "\t" +
        causeOffset + "\t" +
        flatGrounding1 + "\t" +
        compositionalGrounding1 + "\n"

    pw.print(row1)

//    val flatGrounding1 = OntologyHandler.reground(causeOffset)
//    val compositionalGrounding1 = "TBD"

//    val flatGrounding2 = OntologyHandler.
//    val compositionalGrounding2 = "TBD"



//    val row1 =
//      index + "\t" +
//      sentence.trim() + "\t" +
//      cause + "\t" +
//      causeOffset + "\t" +
//      flatGrounding1 + "\t" +
//      compositionalGrounding1 + "\n"

//    val row2 = index + "\t" +
//      sentence.trim + "\t" +
//      effect + "\t" +
//      effectOffset + "\t" +
//      flatGrounding2 + "\t" +
//      compositionalGrounding2 + "\n"

//    pw.print(row1)
//    pw.print(row2)
  }
  pw.close()

}
