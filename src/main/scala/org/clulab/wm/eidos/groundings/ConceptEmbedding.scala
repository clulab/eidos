package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.utils.Namer

@SerialVersionUID(1000L)
case class ConceptEmbedding(var namer: Namer, embedding: Array[Float]) extends Serializable
