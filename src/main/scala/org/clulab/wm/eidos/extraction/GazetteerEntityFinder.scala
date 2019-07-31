package org.clulab.wm.eidos.extraction

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin.{ExtractorEngine, Mention, State}
import org.clulab.processors.{Document, Sentence}
import org.clulab.sequences.LexiconNER
import org.clulab.wm.eidos.expansion.Expander
import GazetteerEntityFinder.NER_OUTSIDE
import org.clulab.wm.eidos.utils.DisplayUtils

/**
  * The GazetteerEntityFinder finds mentions of gazetteer elements.  The matching uses a processors LexiconNER,
  * so the lexicons are provided as paths to the csv files (stored in `resources`) and matching is based on exact
  * string match.  The found mentions are odin TextBoundMentions, where the Mention label is the same as the base
  * name of the gazetteer that matched it.
  * @param lexicons
  * @param expander
  */
class GazetteerEntityFinder(lexicons: Seq[String], expander: Option[Expander]) extends Finder {
  // todo: what happens if more than one gazetteer matches?
  val gazetteers = LexiconNER(lexicons, caseInsensitiveMatching = true)

  /**
    * Annotate the processors Sentence with found matches from the gazetteers in place.
    * The annotations will be placed in the `entities` field, using BIO notation (i.e., B-X, I-X, O, ...)
    * @param s Sentence to be annotated
    */
  def annotateSentence(s: Sentence): Unit = {
    // Find the gazetteer matches for the current sentence
    val matches = gazetteers.find(s)

    // If a parser does not populate the entities field of a Sentence, just add whatever the gazetteers found
    if (s.entities.isEmpty)
      s.entities = Some(matches)
    // Otherwise, overwrite if we matched
    else {
      val sentenceEntities = s.entities.get

      matches.indices.foreach { index =>
        val matchTag = matches(index)
        // Check to see that there is a match
        if (matchTag != NER_OUTSIDE)
        // Overwrite the previous tag only if the gazetteers found something of interest
          sentenceEntities(index) = matchTag
      }
    }
  }

  /**
    * Find mentions corresponding to gazetteers (using string matching).  The label of the mention will
    * match the name of the gazetteer file which matches it.  The matching is done using the LexiconNER
    * class in Processors.
    * @param doc Processors Document, already annotated.  The named entities field does not need to be populated.
    * @return the mentions corresponding to the gazetteer items
    */
  def find(doc: Document, initialState: State = new State(), finderArguments: Option[FinderArguments] = None): Seq[Mention] = {
    // Annotate the document for gazetteer elements
    doc.sentences.map(annotateSentence)
    // Find them and convert them to mentions
    val mentions = for {
      label <- gazetteers.getLabels
      ruleTemplate =
      s"""
         | - name: gazetteer
         |   label: ${label}
         |   priority: 1
         |   type: token
         |   pattern: |
         |       [entity="B-${label}"] [entity="I-${label}"]* (to [entity="B-${label}"] [entity="I-${label}"]*)?
         |
        """.stripMargin
      engine = ExtractorEngine(ruleTemplate)
    } yield engine.extractFrom(doc, initialState)
    val flattened = mentions.flatten
//    DisplayUtils.displayMentions(flattened, doc)
    expander.map(_.expand(flattened)).getOrElse(flattened)
  }
}

object GazetteerEntityFinder {
  val NER_OUTSIDE = "O"

  def apply(lexicons: Seq[String], expander: Option[Expander]) = new GazetteerEntityFinder(lexicons, expander)
  def fromConfig(config: Config) = {
    val lexicons = config[List[String]]("gazetteers.lexicons")
    val expanderConfig = config.get[Config]("gazetteers.expander")
    val expander: Option[Expander] = expanderConfig.map(Expander.fromConfig)
    GazetteerEntityFinder(lexicons, expander)
  }
}
