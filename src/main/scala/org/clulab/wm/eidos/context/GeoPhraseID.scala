package org.clulab.wm.eidos.context

@SerialVersionUID(1L)
case class GeoPhraseID(phraseID: String, PhraseGeoID: Option[Int], StartOffset_locs: Int, EndOffset_locs: Int)
