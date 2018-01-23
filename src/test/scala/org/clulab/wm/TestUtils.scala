package org.clulab.wm

import org.scalatest._

import org.clulab.odin.Attachment
import org.clulab.odin.Mention

class AgroTest extends FlatSpec with Matchers

/**
  * These are the functions that we'll be testing, that import from PaperReader
  */

//val eeWithActionsAndGlobal = ExtractorEngine(rules, myActions, myGlobalAction)
object TestUtils {
  protected val tagName = "org.clulab.wm"
  object Keith extends Tag(tagName)
  object Becky extends Tag(tagName)
  
  val agroSystem = new AgroSystem()

  def extractMentions(text: String): Vector[Mention] = agroSystem.extractFrom(text)
  
  
  def newNodeSpec(nodeText: String, attachments: Set[Attachment]) =
      new NodeSpec(nodeText, attachments)
  def newNodeSpec(nodeText: String, attachments: Attachment*) =
      new NodeSpec(nodeText, attachments.toSet)
  
  def newEdgeSpec(cause: NodeSpec) =
      new EdgeSpec(cause, Set())
  def newEdgeSpec(cause: NodeSpec, effects: NodeSpec*) =
      new EdgeSpec(cause, effects.toSet)
  
  
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
  
  val successful = Seq()
}
