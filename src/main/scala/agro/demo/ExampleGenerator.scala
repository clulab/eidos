package agro.demo

import agro.demo.RAPShell.ieSystem
import org.clulab.wm.AgroSystem
import utils.DisplayUtils.displayMentions

object ExampleGenerator extends App {

  // creates an extractor engine using the rules and the default actions
  val ieSystem = new AgroSystem()
  val proc = ieSystem.proc

  val text = "The government promotes improved cultivar to boost agricultural production for ensuring food security. However, the policy to seriously cut down the use of inorganic fertilizer and phase out the fertilizer subsidy results in deteriorating biophysical conditions, less use of inorganic fertilizer, less water, significantly reduced farm sizes which lead to low benefit from the improved cultivar."
  val doc = proc.annotate(text)

  // extract mentions from annotated document
  val mentions = ieSystem.extractFrom(doc).sortBy(m => (m.sentence, m.getClass.getSimpleName))

  // debug display the mentions
  displayMentions(mentions, doc)

  // serialize the mentions to a json file


}
