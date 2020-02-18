package org.clulab.wm.eidos.apps
import org.clulab.wm.eidos.utils.{FileUtils}
import java.io.{File, PrintWriter}

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

  for (entry <- lines) {
    val line = entry.split("\t")
//    println(line.mkString(" "))

    val index = line(0)
    val sentence = line(18)

    val cause = line(4)
    val causeStartOffset = sentence indexOf cause
    val causeEndOffset = causeStartOffset+cause.length
//    println(cause)

    val effect = line(11)
    val effectStartOffset = sentence indexOf effect
    val effectEndOffset = effectStartOffset+cause.length
//    println(index+"\n"+sentence+"\n"+cause+"\n"+effect)

    val flatGrounding = "TBD"
    val compositionalGrounding = "TBD"

    val row1 =
      index + "\t" +
      sentence.trim() + "\t" +
      cause + "\t" +
      (causeStartOffset,causeEndOffset) + "\t" +
      flatGrounding + "\t" +
      compositionalGrounding + "\n"

    val row2 = index + "\t" +
      sentence.trim + "\t" +
      effect + "\t" +
      (effectStartOffset,effectEndOffset) + "\t" +
      flatGrounding + "\t" +
      compositionalGrounding + "\n"

    pw.print(row1)
    pw.print(row2)
  }
  pw.close()

}
