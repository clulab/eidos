package org.clulab.wm.eidos.test

import scala.collection.Seq
import org.scalatest._
import org.clulab.odin.{Attachment, Mention}
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.text.NodeSpec
import org.clulab.wm.eidos.text.EdgeSpec

/**
  * These are the functions that we'll be testing, that import from PaperReader
  */

//val eeWithActionsAndGlobal = ExtractorEngine(rules, myActions, myGlobalAction)
object TestUtils {
  
  // This will be a GraphTest in contrast to a RuleTest
  class Test extends FlatSpec with Matchers {

    class TesterTag extends Tag("org.clulab.wm.TestUtils")
    
    object Nobody   extends TesterTag
    object Somebody extends TesterTag
    object Keith    extends TesterTag
    object Becky    extends TesterTag
    object Egoitz   extends TesterTag
    object Ajay     extends TesterTag
    object Adarsh   extends TesterTag
    object Mithun   extends TesterTag
    object Fan      extends TesterTag
    object Zheng    extends TesterTag
    object Mihai    extends TesterTag
    object Ben      extends TesterTag
    object Heather  extends TesterTag
    object Vikas    extends TesterTag
    
    val passingTest = it
    val failingTest = ignore
    val brokenSyntaxTest = ignore
    val futureWorkTest = ignore // added to mark the tests that are not currently passing, but with planned changes to the
                                // framework, they will be achievable
    val inferenceTest = ignore  // type of futureWorkTest -- added for tests which are now failing because of entity
                                // filtering, basically because inference or coref would be needed
    val tempBrokenEntitiesTest = ignore
    val affectEventTest = ignore

    val successful = Seq()
    
    class Tester(text: String) {
      //val mentions = extractMentions(clean(text))
      val mentions = EidosMention.findReachableMentions(extractMentions(clean(text)))
      
      def getSpecialChars(s: String) = s.filter(c => c < 32 || 127 < c)
      
      def clean(messyText: String): String = {
        val cleanText = messyText
            .replace('|', ' ') // before trim so space will be trimmed if necessary
            .trim()
            .replace('\n', ' ')
            .replace('\r', ' ')
            .replace('\t', ' ')
            .replaceAll("  +", " ")
        val specialChars = getSpecialChars(cleanText)
        
        if (!specialChars.isEmpty())
          throw new IllegalArgumentException("Text contained a special chars: " + specialChars)
        cleanText
      }
      
      protected def toString(mentions: Seq[Mention]): String = {
        val stringBuilder = new StringBuilder()
        
        mentions.indices.foreach(index => stringBuilder.append(s"${index}: ${mentions(index).text}\n"))
        stringBuilder.toString()
      }
    
      protected def annotateTest(result: Seq[String]): Seq[String] =
          if (result == successful)
            result
          else
            result ++ Seq("Mentions:\n" + toString(mentions))
      
      def test(nodeSpec: NodeSpec): Seq[String] = annotateTest(nodeSpec.test(mentions))
      
      def test(edgeSpec: EdgeSpec): Seq[String] = annotateTest(edgeSpec.test(mentions))
    }    
  }
  
  lazy val ieSystem = new EidosSystem()

  def extractMentions(text: String): Seq[Mention] = ieSystem.extractFromText(text, cagRelevantOnly = false).odinMentions
}
