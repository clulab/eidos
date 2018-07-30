package org.clulab.wm.eidos.groundings

@SerialVersionUID(1000L)
case class ConceptEmbedding(var concept: String, embedding: Array[Double]) extends Serializable
