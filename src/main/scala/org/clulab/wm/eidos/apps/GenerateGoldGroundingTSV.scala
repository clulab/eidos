package org.clulab.wm.eidos.apps
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.groundings.OntologyHandler
import java.io.PrintWriter
import org.clulab.struct.Interval
import org.clulab.wm.eidos.EidosSystem

// this app creates a spreadsheet to be filled in with gold groundings

object GenerateGoldGroundingTSV extends App {

  // load tsv files from resources
  val originalFile = FileUtils.getTextFromFile("src/main/resources/org/clulab/wm/eidos/english/grounding/groundingEvalEntities.tsv")
  val fileAsString: String = originalFile.toString
  val lines: Array[String] = fileAsString.split("\n")

  var outFilename = "src/main/resources/org/clulab/wm/eidos/english/grounding/gold_groundings.tsv"
  var rejectsFilename = "src/main/resources/org/clulab/wm/eidos/english/grounding/rejected_sentences.tsv"

  val header =
    "GOLD Annotated?\t" +
    "Index\t" +
    "Sentence\t" +
    "Entity\t" +
    "Character Offsets\t" +
    "Cause/Effect\t" +
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

  val pwRejects = new PrintWriter(rejectsFilename)

  pwRejects.println(lines.head)

  val ontologyHandler: OntologyHandler = new
      EidosSystem().components.ontologyHandler
  val ieSystem = new EidosSystem()

  for (entry <- lines.tail) {
    val line = entry.split("\t")

    val index = line(0)
    val sentence = line(18)

    val cause = line(4)
    val causeStartOffset = sentence indexOf cause
    val causeEndOffset = causeStartOffset+cause.length
    val causeOffset: Interval = Interval(causeStartOffset,causeEndOffset)

    val effect = line(11)
    val effectStartOffset = sentence indexOf effect
    val effectEndOffset = effectStartOffset+effect.length
    val effectOffset: Interval = Interval(effectStartOffset,effectEndOffset)

    val document = ieSystem.annotate(sentence)

    if (causeStartOffset != -1) {

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

      val row1 =
        "" + "\t" +
        index + "\t" +
        sentence.trim() + "\t" +
        cause + "\t" +
        (causeOffset.start, causeOffset.end) + "\t" +
        "cause" + "\t" +
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
    else {
      pwRejects.print(line.mkString("\t")+"\n")
    }

    if (effectStartOffset != -1) {

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

      val row2 =
        "" + "\t" +
        index + "\t" +
        sentence.trim() + "\t" +
        effect + "\t" +
        (effectOffset.start, effectOffset.end) + "\t" +
        "effect" + "\t" +
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
    else {
      pwRejects.print(line.mkString("\t")+"\n")
    }
  }
  pw.close()
  pwRejects.close()
}
