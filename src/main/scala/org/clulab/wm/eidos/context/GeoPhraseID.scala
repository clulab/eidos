package org.clulab.wm.eidos.context

@SerialVersionUID(1L)
case class GeoPhraseID(text: String, geonameID: Option[Int], startOffset: Int, endOffset: Int)
