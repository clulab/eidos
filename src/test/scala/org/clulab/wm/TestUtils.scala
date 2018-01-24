package org.clulab.wm

import org.scalatest._

import org.clulab.odin.Attachment
import org.clulab.odin.Mention

import org.clulab.wm.Aliases.Quantifier

/**
  * These are the functions that we'll be testing, that import from PaperReader
  */

//val eeWithActionsAndGlobal = ExtractorEngine(rules, myActions, myGlobalAction)
object TestUtils {
  class Test extends FlatSpec with Matchers {
    val passingTest = it
    val failingTest = ignore
  }
  
  protected val tagName = "org.clulab.wm"
  object Nobody extends Tag(tagName)
  object Somebody extends Tag(tagName)
  object Keith extends Tag(tagName)
  object Becky extends Tag(tagName)
  // TODO: Add other users
  
  protected val system = new AgroSystem() // TODO: Change this class name

  def extractMentions(text: String): Seq[Mention] = system.extractFrom(text)
  
  class Tester(text: String) {
    val mentions = extractMentions(text)
    
    def test(nodeSpec: NodeSpec): Seq[String] = nodeSpec.test(mentions)
    
    def test(edgeSpec: EdgeSpec): Seq[String] = edgeSpec.test(mentions)
  }
  
  def newNodeSpec(nodeText: String, attachments: Set[Attachment]) =
      new NodeSpec(nodeText, attachments)
  def newNodeSpec(nodeText: String, attachments: Attachment*) =
      new NodeSpec(nodeText, attachments.toSet)
  
  def newEdgeSpec(cause: NodeSpec, event: Event, effects: NodeSpec*) =
      new EdgeSpec(cause, event, effects.toSet)
  
  
  def newQuantification(quantifier: String) =
      new Quantification(quantifier)
  
  def newDecrease(trigger: String) =
      new Decrease(trigger, None)
  def newDecrease(trigger: String, quantifiers: String*) =
      new Decrease(trigger, Option(quantifiers.toSeq))
          
  def newIncrease(trigger: String) =
      new Increase(trigger, None)
  def newIncrease(trigger: String, quantifiers: String*) =
      new Increase(trigger, Option(quantifiers.toSeq))

  def newUnmarked(quantifier: String) =
      new Unmarked(quantifier)
  
  val successful = Seq()
}
