package org.clulab.wm.eidos.exporters

import ai.lum.common.StringUtils.StringWrapper
import org.clulab.wm.eidos.groundings.grounders.EidosOntologyGrounder
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.groundings.GroundingUtils.getBaseGroundingString
import org.clulab.wm.eidos.groundings.GroundingUtils.getGroundingsString

case class EntityInfo(
  m: EidosMention,
  groundAs: Seq[String] = Seq(EidosOntologyGrounder.PRIMARY_NAMESPACE),
  topN: Int = 5,
  delim: String = ", "
) {
  val text: String = m.odinMention.text
  val canonicalName: String = m.canonicalName
  val norm: String = getBaseGroundingString(m)
  //  val modifier: String = ExportUtils.getModifier(m)
  //  val polarity: String = ExportUtils.getPolarity(m)
  val groundingStrings: Seq[String] = groundAs.map { namespace =>
    getGroundingsString(m, namespace, topN, delim)
  }

  // modifier and polarity are not calculated
  def toTSV: Seq[String] = Seq(text, norm, "", "").map(_.normalizeSpace)

  def groundingToTSV: Seq[String] = groundingStrings.map(_.normalizeSpace)
}
