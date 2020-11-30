package org.clulab.wm.eidos.components

import org.clulab.wm.eidoscommon.EidosProcessor
import org.clulab.wm.eidos.actions.CorefHandler
import org.clulab.wm.eidos.attachments.AttachmentHandler
import org.clulab.wm.eidos.attachments.HypothesisHandler
import org.clulab.wm.eidos.attachments.NegationHandler
import org.clulab.wm.eidos.context.GeoNormFinder
import org.clulab.wm.eidos.context.SeasonFinder
import org.clulab.wm.eidos.context.TimeNormFinder
import org.clulab.wm.eidos.document.EidosSentenceClassifier
import org.clulab.wm.eidos.expansion.ConceptExpander
import org.clulab.wm.eidos.expansion.MostCompleteEventsKeeper
import org.clulab.wm.eidos.expansion.NestedArgumentExpander
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.groundings.OntologyHandler
import org.clulab.wm.eidos.groundings.grounders.AdjectiveGrounder
import org.clulab.wm.eidos.utils.StopwordManager

case class EidosComponents(
  // proc
  procOpt: Option[EidosProcessor],
  // finder
  findersOpt: Option[Seq[Finder]],
  // odin
  attachmentHandlerOpt: Option[AttachmentHandler],
  conceptExpanderOpt: Option[ConceptExpander],
  corefHandlerOpt: Option[CorefHandler],
  hedgingHandlerOpt: Option[HypothesisHandler],
  mostCompleteEventsKeeperOpt: Option[MostCompleteEventsKeeper],
  negationHandlerOpt: Option[NegationHandler],
  nestedArgumentExpanderOpt: Option[NestedArgumentExpander],
  stopwordManagerOpt: Option[StopwordManager],
  // eidos
  adjectiveGrounderOpt: Option[AdjectiveGrounder],
  eidosSentenceClassifierOpt: Option[EidosSentenceClassifier],
  ontologyHandlerOpt: Option[OntologyHandler]
) {
  lazy val geoNormFinderOpt: Option[GeoNormFinder] = findersOpt.flatMap(_.collectFirst { case f: GeoNormFinder => f })
  lazy val useGeoNorm: Boolean = geoNormFinderOpt.isDefined

  lazy val timeNormFinderOpt: Option[TimeNormFinder] = findersOpt.flatMap(_.collectFirst { case f: TimeNormFinder => f })
  lazy val useTimeNorm: Boolean = timeNormFinderOpt.isDefined

  lazy val seasonFinderOpt: Option[SeasonFinder] = findersOpt.flatMap(_.collectFirst { case f: SeasonFinder => f })
  lazy val languageOpt: Option[String] = procOpt.map(_.language)

  def filter(componentOpts: ComponentOpts): EidosComponents = {
    EidosComponents(
      // proc
      procOpt.filter(_ => componentOpts.proc),
      // finder
      findersOpt.filter(_ => componentOpts.finders),
      // odin
      attachmentHandlerOpt.filter(_ => componentOpts.attachmentHandler),
      conceptExpanderOpt.filter(_ => componentOpts.conceptExpander),
      corefHandlerOpt.filter(_ => componentOpts.corefHandler),
      hedgingHandlerOpt.filter(_ => componentOpts.hedgingHandler),
      mostCompleteEventsKeeperOpt.filter(_ => componentOpts.mostCompleteEventsKeeper),
      negationHandlerOpt.filter(_ => componentOpts.negationHandler),
      nestedArgumentExpanderOpt.filter(_ => componentOpts.nestedArgumentExpander),
      stopwordManagerOpt.filter(_ => componentOpts.stopwordManager),
      // eidos
      adjectiveGrounderOpt.filter(_ => componentOpts.adjectiveGrounder),
      eidosSentenceClassifierOpt.filter(_ => componentOpts.eidosSentenceClassifier),
      ontologyHandlerOpt.filter(_ => componentOpts.ontologyHandler)
    )
  }
}
