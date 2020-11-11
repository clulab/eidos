package org.clulab.wm.eidos.apps

import org.clulab.processors.fastnlp.FastNLPProcessorWithSemanticRoles
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.meta.CdrText

import scala.util.Random

object ExtractSemanticRoles extends App {
  val inputDir = args(0)

  val files = FileUtils.findFiles(inputDir, "json")
  val processor = new FastNLPProcessorWithSemanticRoles {
    annotate("This is a test.") // prime the pump
  }
  val texts = files.map { file =>
    val eidosText = CdrText(file)
    val text = eidosText.getText
    file.getName -> text
  }.toMap
  val fileNames = texts.keys

  while (true) {
    val shuffledNames = Random.shuffle(fileNames)
    shuffledNames.par.foreach { fileName =>
      println(s"Extracting from ${fileName}")
      val text = texts(fileName)
      processor.annotate(text, true)
    }
  }
}
