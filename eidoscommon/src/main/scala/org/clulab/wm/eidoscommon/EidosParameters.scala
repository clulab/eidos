package org.clulab.wm.eidoscommon

object EidosParameters {
  // EidosSystem
  // Taxonomy relations that should make it to final causal analysis graph
  val           CAUSAL_LABEL = "Causal"
  val          CONCEPT_LABEL = "Concept"
  val CONCEPT_EXPANDED_LABEL = "Concept-Expanded"
  val             CORR_LABEL = "Correlation"
  val            COREF_LABEL = "Coreference"
  val        MIGRATION_LABEL = "HumanMigration"
  // Taxonomy relations for other uses
  val         RELATION_LABEL = "EntityLinker"

  // CAG filtering
  val CAG_EDGES: Set[String] = Set(CAUSAL_LABEL, CONCEPT_EXPANDED_LABEL, CORR_LABEL, COREF_LABEL)
  val EXPAND: Set[String] = CAG_EDGES ++ Set(MIGRATION_LABEL)
}
