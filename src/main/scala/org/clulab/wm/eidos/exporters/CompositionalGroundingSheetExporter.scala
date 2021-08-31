package org.clulab.wm.eidos.exporters


import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.groundings.grounders.SRLCompositionalGrounder
import org.clulab.wm.eidos.groundings.{ConceptPatterns, IndividualGrounding, OntologyGrounding, PredicateGrounding, SingleOntologyNodeGrounding}
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidoscommon.utils.Closer._
import org.clulab.wm.eidoscommon.utils.{FileUtils, StringUtils}
import org.clulab.wm.ontologies.PosNegTreeDomainOntology.PosNegTreeDomainOntologyBuilder

import scala.collection.mutable.ArrayBuffer

class CompositionalGroundingSheetExporter(filename: String, reader: EidosSystem, config: Config) extends Exporter {

  private val currHandler = reader.components.ontologyHandlerOpt.get
  val currOntologyGrounder = currHandler.ontologyGrounders
    .collectFirst{ case srl: SRLCompositionalGrounder => srl }
    .getOrElse(throw new RuntimeException("Need to have the SRL grounding enabled"))
  private val w2v = currHandler.wordToVec
  private val canonicalizer = currHandler.canonicalizer
  private val proc = reader.components.procOpt.get
  private val builder = new PosNegTreeDomainOntologyBuilder(proc, canonicalizer, filter = true)
  private val ontology = builder.buildFromPath(config.getString("groundinginsight.ontologyPath"))
  private val nodes = ontology.ontologyNodes.map(n => (n.fullName, n)).toMap
  private val k: Int = config.getInt("groundinginsight.topk")




  override def export(annotatedDocument: AnnotatedDocument): Unit = {

    FileUtils.printWriterFromFile(filename + ".insight_sheet.tsv").autoClose { pw =>

      pw.println("sentence\tcause\tc theme\tscore\tc prop\tscore\tc proc\tscore\tc proc prop\tscore\teffect\te theme\tscore\te prop\tscore\te proc\tscore\te proc prop")

      val doc = annotatedDocument.document

      val mentionsPerSentence = annotatedDocument.eidosMentions.groupBy(_.odinMention.sentence)

      val sentIDs = mentionsPerSentence.keys.toSeq.sorted

      for (i <- sentIDs) {
        val sentence = doc.sentences(i).getSentenceText.trim()
        val srls = doc.sentences(i).enhancedSemanticRoles.getOrElse(None)
        val deps = doc.sentences(i).dependencies.get
        val mentions = mentionsPerSentence(i)

        mentions.filter(_.odinMention matches "Causal").foreach { em =>
          val cause = em.eidosArguments("cause").headOption.getOrElse(throw new RuntimeException("no cause!"))
          val cText = cause.odinMention.text
          val cInterval = cause.tokenIntervals
          val ctheme = mentionGroundingInfo(cause).head
          val cthemeS = mentionGroundingInfo(cause)(1)
          val cthemeProp = mentionGroundingInfo(cause)(2)
          val cthemePropS = mentionGroundingInfo(cause)(3)
          val cthemeProc = mentionGroundingInfo(cause)(4)
          val cthemeProcS = mentionGroundingInfo(cause)(5)
          val cthemeProcProp = mentionGroundingInfo(cause)(6)
          val cthemeProcPropS = mentionGroundingInfo(cause)(7)

          val effect = em.eidosArguments("effect").headOption.getOrElse(throw new RuntimeException("no effect!"))
          val eText = effect.odinMention.text
          val eInterval = effect.tokenIntervals
          val etheme = mentionGroundingInfo(effect).head
          val ethemeS = mentionGroundingInfo(effect)(1)
          val ethemeProp = mentionGroundingInfo(effect)(2)
          val ethemePropS = mentionGroundingInfo(effect)(3)
          val ethemeProc = mentionGroundingInfo(effect)(4)
          val ethemeProcS = mentionGroundingInfo(effect)(5)
          val ethemeProcProp = mentionGroundingInfo(effect)(6)
          val ethemeProcPropS = mentionGroundingInfo(effect)(7)

          val line = sentence + "\t" + cText + "\t" + cInterval + "\t" + ctheme + "\t" + cthemeS + "\t" + cthemeProp + "\t" + cthemePropS + "\t" + cthemeProc + "\t" + cthemeProcS + "\t" + cthemeProcProp + "\t" + cthemeProcPropS + "\t" + eText + "\t" + eInterval + "\t" + etheme + "\t" + ethemeS + "\t" + ethemeProp + "\t" + ethemePropS + "\t" + ethemeProc + "\t" + ethemeProcS + "\t" + ethemeProcProp + "\t" + ethemeProcPropS

          pw.println(line)


        }
      }
    }
  }

  def mentionGroundingInfo(m: EidosMention): ArrayBuffer[String] = {

    val compGrounding = m.grounding.get("wm_compositional")
    val firstCompGrounding = compGrounding.head.grounding.headOption.get
    val pred_grounds = new ArrayBuffer[String]

    firstCompGrounding match {
      case comp: PredicateGrounding =>
        val tuple = comp.predicateTuple
        // theme
        val theme = tuple.theme.headName.getOrElse("NONE")
        val theme_score = if (tuple.theme.headOption.nonEmpty) tuple.theme.headOption.get.score else 0f
        // theme_properties
        val themeProp = tuple.themeProperties.headName.getOrElse("NONE")
        val themeProp_score = if (tuple.themeProperties.headOption.nonEmpty) tuple.themeProperties.headOption.get.score else 0f
        // theme_process
        val themeProc = tuple.themeProcess.headName.getOrElse("NONE")
        val themeProc_score = if (tuple.themeProcess.headOption.nonEmpty) tuple.themeProcess.headOption.get.score else 0f
        // theme_process_props
        val themeProcProp = tuple.themeProcessProperties.headName.getOrElse("NONE")
        val themeProcProp_score = if (tuple.themeProcessProperties.headOption.nonEmpty) tuple.themeProcessProperties.headOption.get.score else 0f
        pred_grounds.append(theme, theme_score.toString, themeProp, themeProp_score.toString, themeProc, themeProc_score.toString, themeProcProp, themeProcProp_score.toString)
    }
    println(pred_grounds)

    pred_grounds
  }

  def exactMatch(text: String): Seq[String] = {
    val patterns: Seq[ConceptPatterns] = currOntologyGrounder.conceptPatterns
    val lowerText = text.toLowerCase
    val exactMatches = patterns.filter(pattern => StringUtils.afterLast(pattern.namer.name, '/', true) == lowerText)
    if (exactMatches.nonEmpty)
      exactMatches.map(exactMatch => s"         Exact Match: ${exactMatch.namer}\t(1.0f)")
    else {
      val matchedPatterns = currOntologyGrounder.nodesPatternMatched(text, patterns)
      if (matchedPatterns.nonEmpty)
        matchedPatterns.map(grounding => s"         Pattern Match: ${grounding.namer}\t(1.0f)")
      else Seq.empty
    }
  }

  def examplesDetail(text: String, examples: Seq[String]): Seq[String] = {
    val lines = new ArrayBuffer[String]()
    val scoredExamples = examples
      .map(example => (example, w2v.stringSimilarity(text, example)))
      .sortBy(- _._2)
    val bestMatch = scoredExamples.head
    val worstMatch = scoredExamples.last
    val topk = scoredExamples.take(k)
    val avg = scoredExamples.map(_._2).sum / scoredExamples.length.toFloat

    lines.append(s"       num examples: ${scoredExamples.length}")
    lines.append(s"       examples with top score:")
    val topKLines = topk.map(t => s"          --> ${t._1}\t(${t._2})")
    lines.appendAll(topKLines)
    lines.append(s"       max match: ${bestMatch._1} (${bestMatch._2})")
    lines.append(s"       min match: ${worstMatch._1} (${worstMatch._2})")
    lines.append(s"       avg match: ${avg})")

    lines
  }

}
