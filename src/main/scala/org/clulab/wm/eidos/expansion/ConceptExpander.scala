package org.clulab.wm.eidos.expansion

import org.clulab.odin.Attachment
import org.clulab.odin.Mention
import org.clulab.odin.State
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments.{Decrease, Increase, NegChange, PosChange, Quantification}
import org.clulab.wm.eidos.mentions.MentionUtils
import org.clulab.wm.eidoscommon.EidosParameters

class ConceptExpander(expanderOpt: Option[Expander], keepStatefulConcepts: Boolean) extends Expander {

  // If enabled and applicable, expand Concepts which don't participate in primary events
  def expand(mentions: Seq[Mention], avoidState: State = new State()): Seq[Mention] = {

    def isExpandableMod(a: Attachment): Boolean =
      a.isInstanceOf[Increase] ||
        a.isInstanceOf[Decrease] ||
        a.isInstanceOf[Quantification] ||
        a.isInstanceOf[PosChange] ||
        a.isInstanceOf[NegChange]

    def expandIfNotExpanded(ms: Seq[Mention], expandedState: State): Seq[Mention] = {
      // Get only the Concepts that don't overlap with a previously expanded Concept...
      // todo: note this filter is based on token interval overlap, perhaps a smarter way is needed (e.g., checking the argument token intervals?)
      val notYetExpanded = ms.filter(m => expandedState.mentionsFor(m.sentence, m.tokenInterval).isEmpty)
      // Expand
      val expanded = expanderOpt.get.expand(notYetExpanded, new State())
      // Modify the label to flag them for keeping
      val relabeled = expanded.map(m => MentionUtils.withLabel(m, EidosParameters.CONCEPT_EXPANDED_LABEL))

      relabeled
    }

    // Check to see if we are keeping stateful concepts and if we have an expander
    if (!keepStatefulConcepts || expanderOpt.isEmpty)
      mentions
    else {
      // Split the mentions into Cpncepts and Relations by the label
      val (concepts, relations) = mentions.partition(_ matches EidosParameters.CONCEPT_LABEL)
      // Check to see if any of the Concepts have state attachments
      val (expandable, notExpandable) = concepts.partition(_.attachments.exists(isExpandableMod))
      // Get the already expanded mentions for this document
      val prevExpandableState = State(relations.filter(rel => EidosParameters.CAG_EDGES.contains(rel.label)))
      // Expand the Concepts if they weren't already part of an expanded Relation
      val expandedConcepts = expandIfNotExpanded(expandable, prevExpandableState)
      expandedConcepts ++ notExpandable ++ relations
    }
  }
}
