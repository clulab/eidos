package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.utils.Namer

import scala.util.matching.Regex

@SerialVersionUID(1000L)
case class ConceptEmbedding(val namer: Namer, embedding: Array[Float]) extends Serializable

@SerialVersionUID(1000L)
case class ConceptPatterns(val namer: Namer, patterns: Option[Array[Regex]]) extends Serializable
