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

class GroundingInsightExporter(filename: String, reader: EidosSystem) extends Exporter {

  val config: Config = ConfigFactory.load()


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

    FileUtils.printWriterFromFile(filename + ".insight.txt").autoClose { pw =>

      val doc = annotatedDocument.document
//      println("DOC SENTENCES")
//      for (i <- doc.sentences) {
//        println("\t" + i + "\t" + i.getSentenceText)
//      }
      val mentionsPerSentence = annotatedDocument.eidosMentions.groupBy(_.odinMention.sentence)
//      println("\nMentionsPerSentence:")
      val sentIDs = mentionsPerSentence.keys.toSeq.sorted
//      for (m <- mentionsPerSentence) {
//        println("START GROUP "+m._1)
//        for (x <- m._2) {
//          println("\t"+x.canonicalName)
//        }
//      }
      for (i <- sentIDs) {
        pw.println("********************************************\n")
        pw.println(s"Sentence $i:\t${doc.sentences(i).getSentenceText.strip()}\n\n")
        pw.println("SRLs:")
        pw.println(doc.sentences(i).enhancedSemanticRoles.getOrElse(None))
        pw.println("\nDEPs:")
        pw.println(doc.sentences(i).dependencies.get)
//        println("KEY?:\t"+i)
        val mentions = mentionsPerSentence(i)

        mentions.filter(_.odinMention matches "Causal").foreach { em =>
          val cause = em.eidosArguments("cause").headOption.getOrElse(throw new RuntimeException("no cause!"))
          pw.println("\nCAUSE:\n")
          pw.println(mentionGroundingInfo(cause))

          val effect = em.eidosArguments("effect").headOption.getOrElse(throw new RuntimeException("no effect!"))
          pw.println("\nEFFECT:\n")
          pw.println(mentionGroundingInfo(effect))

        }
      }
    }
  }

  def mentionGroundingInfo(m: EidosMention): String = {
    val lines = new ArrayBuffer[String]()
    val text = m.odinMention.text
    lines.append(s"mention text: ${text}\n")
    lines.append(s"mention entities: ${m.odinMention.entities.get}\t")

    val canonical = m.canonicalName
    val compGrounding = m.grounding.get("wm_compositional")
    compGrounding.foreach{ grounding =>
      lines.appendAll(groundingInfo(grounding, canonical))
    }
    lines.mkString("\n")
  }


  def groundingInfo(grounding: OntologyGrounding, canonical: String): Seq[String] = {
    grounding.grounding.map(groundingInfo(_, canonical))
  }

  def groundingInfo(grounding: IndividualGrounding, canonical: String): String = {
    val lines = new ArrayBuffer[String]

    grounding match {
      case single: SingleOntologyNodeGrounding =>
        val node = nodes(grounding.name)
        lines.append("--------GROUNDING INFO----------")
        lines.append(s"  NODE: ${single.name}")
        lines.append("")
        lines.append(s"     score: ${single.score}")
        lines.append(s"     exact matches and regex:")
        lines.appendAll(exactMatch(canonical))
        lines.append("")
        lines.append(s"     Positive examples:")
        lines.appendAll(examplesDetail(canonical, node.getPosValues))
        lines.append("")
        val negExamples = node.getNegValues
        if (negExamples.nonEmpty) {
          lines.append(s"     Negative examples:")
          lines.appendAll(examplesDetail(canonical, negExamples))
          lines.append("")
        }
        lines.append("")
        lines.mkString("\n")
      case comp: PredicateGrounding =>
        val tuple = comp.predicateTuple
        // theme
        lines.append("\n===== Theme =====")
        lines.appendAll(groundingInfo(tuple.theme, canonical))
        // theme_properties
        lines.append("===== Theme Properties =====")
        lines.appendAll(groundingInfo(tuple.themeProperties, canonical))
        // theme_process
        lines.append("===== Theme Process =====")
        lines.appendAll(groundingInfo(tuple.themeProcess, canonical))
        // theme_process_props
        lines.append("===== Theme Process Props =====")
        lines.appendAll(groundingInfo(tuple.themeProcessProperties, canonical))
        lines.mkString("\n")
      case _ => ???
    }

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
    topk.map(t => s"          --> ${t._1}\t(${t._2})")
    lines.append(s"       max match: ${bestMatch._1} (${bestMatch._2})")
    lines.append(s"       min match: ${worstMatch._1} (${worstMatch._2})")
    lines.append(s"       avg match: ${avg})")

    lines
  }

}
