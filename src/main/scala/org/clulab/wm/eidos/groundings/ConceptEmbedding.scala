package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidoscommon.utils.Namer

import scala.util.matching.Regex

@SerialVersionUID(1000L)
case class ConceptEmbedding(namer: Namer, embedding: Array[Float]) extends Serializable

@SerialVersionUID(1000L)
case class ConceptPatterns(namer: Namer, patterns: Option[Array[Regex]]) extends Serializable
