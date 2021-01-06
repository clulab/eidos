package org.clulab.wm.eidos.exporters

import org.clulab.wm.eidos.{EidosCluProcessor, EidosSystem}
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.groundings.PredicateGrounding
import org.clulab.wm.eidos.utils.Closer._
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.FileUtils.printWriterFromFile

class DebugGroundingExporter(filename: String, reader: EidosSystem, reground: Boolean = true) extends Exporter {

  // FIXME: the regound is never being passed, will always be true
  override def export(annotatedDocument: AnnotatedDocument): Unit = {
    if (reground) {
      val ontologyHandler = reader.components.ontologyHandler
      // Reground
      annotatedDocument.allEidosMentions.foreach { eidosMention =>
        ontologyHandler.ground(eidosMention)
      }
    }

    // SRLS
    // top 5 for each slot
    // top 5 tuples
    FileUtils.printWriterFromFile(filename + ".debug.txt").autoClose { pw =>

//      pw.println("********************************************")
      val doc = annotatedDocument.document
      for (i <- doc.sentences.indices) {
        val clusent = reader.components.proc.asInstanceOf[EidosCluProcessor].cluproc.annotate(doc.sentences(i)
          //asInstanceOf[EidosCluProcessor].cluproc.annotate(doc.sentences(i)
          .getSentenceText).sentences.head
        pw.println("********************************************\n")
        pw.println(s"Sentence $i: ${clusent.getSentenceText}.\n")
        pw.println("SRLS:")
        pw.println(clusent.enhancedSemanticRoles.get)
//        pw.println()
        pw.println("DEPS:")
        pw.println(clusent.dependencies.get)
        val mentions = annotatedDocument.eidosMentions.filter(_.odinMention.sentence == i)

        mentions.filter(_.odinMention matches "Causal").foreach { em =>
          val args = em.eidosArguments("cause") ++ em.eidosArguments("effect")
          val info = args.map(m => (m.odinMention.text, m.grounding.get("wm_compositional"), m.grounding.get("wm_flattened")))
          for ((text, groundingComp, groundingFlat) <- info) {
            if (text=="famine") {
              //do nothing
            }
            else {
              pw.println(s"mention text: ${text}\n")
              pw.println(s"mention entities: ${args.head.odinMention.entities.get}\t")

              pw.println(s"Flat Grounding:")
              groundingFlat match {
                case None => pw.println("no grounding...")
                case Some(grounding) =>
                  grounding.grounding.foreach { gr =>
                    pw.println(s"  --> grounding:  ${gr.name}")
                    pw.println(s"      score:      ${gr.score}")
                  }
              }
              pw.println()

              pw.println(s"Compositional Grounding:")
              groundingComp match {
                case None => pw.println("no grounding...")
                case Some(grounding) =>
                  var tupNum: Int = 0
                  grounding.grounding.foreach { gr =>
                    pw.println(s"Tuple $tupNum")
                    tupNum += 1
                    val tuple = gr.asInstanceOf[PredicateGrounding].predicateTuple
                    val labels = Seq("Theme", "Theme Prop", "Process", "Process Prop")
                    val slots = Seq(tuple.theme, tuple.themeProperties, tuple.themeProcess, tuple.themeProcessProperties)
//                    val theme_name = if (tuple.theme.nonEmpty) slots.head.grounding.head.name else None
                    for (j <- labels.indices) {
                      pw.println(s"\t${labels(j)}")
                      for (g <- slots(j).grounding) {
//                        if (g.name == theme_name && j != 0) {
//                          //do nothing
//                        }
//                        else {
//                          pw.println(s"    --> grounding ${j}:   ${g.name}")
//                          pw.println(s"        score:        ${g.score}")
//                        }
                        pw.println(s"    --> grounding ${j}:   ${g.name}")
                        pw.println(s"        score:        ${g.score}")
                        //                      pw.println()
                      }
                      pw.println()

                    }
                  }
              }
            }

            }

          }

      }
    }




    /**
     * ********************************************
     * Sentence: The flooding caused sadness.
     *
     * SRLS:
     *  [directed graph]
     *
     * Deps:
     *  [directed graph]
     *
     * ---------------------------------
     * flat grounding:
     * ---------------------------------
     *  0: ddd, 0.8
     *  ...
     *
     * ---------------------------------
     * compositional grounding:
     * ---------------------------------
     * Tuple 0:
     *    concept:
     *      abc, 0.9
     *      def, 0.8
     *      ..
     *     concept property:
     *      None
     *     process:
     *      kdslkd, 0.9
     *      ...
     *     process property:
     *       xxx, 1.0
     *
     * [Tuple 1 - 4]
     *
     *
     */


//    FileUtils.printWriterFromFile(filename + ".debug.txt").autoClose { pw =>
//      annotatedDocument.eidosMentions.filter(_.odinMention matches "Causal").foreach { em =>
//        val args = em.eidosArguments("cause") ++ em.eidosArguments("effect")
//        val info = args.map(m => (m.odinMention.text, m.grounding.get("wm_compositional"), m.grounding.get("wm_flattened")))
//        for ((text, groundingComp, groundingFlat) <- info) {
//          pw.println(s"text: ${text}")
//          pw.println(s"Compositional Grounding:")
//          groundingComp match {
//            case None => pw.println("no grounding...")
//            case Some(grounding) =>
//              grounding.grounding.foreach { gr =>
//                pw.println(s"  --> grounding: ${gr.name}")
//                pw.println(s"      score: ${gr.score}")
//              }
//          }
//          pw.println(s"Flat Grounding:")
//          groundingFlat match {
//            case None => pw.println("no grounding...")
//            case Some(grounding) =>
//              grounding.grounding.foreach { gr =>
//                pw.println(s"  --> grounding: ${gr.name}")
//                pw.println(s"      score: ${gr.score}")
//              }
//          }
//          pw.println()
//        }
//
//      }
//    }
  }

}
