package org.clulab.wm.eidoscommon.utils

abstract class TagSet {
  def isAnyNoun(tag: String): Boolean
  def isAnyVerb(tag: String): Boolean
  def isAnyAdjective(tag: String): Boolean

  def isCoordinating(tag: String): Boolean
  def isValidFinal(tag: String): Boolean
  def isStopwordContent(tag: String): Boolean
  def isOntologyContent(tag: String): Boolean
  def isInvalidEdge(tag: String): Boolean
}

/*

1. -LCB- Left  curly brace
2. -RCB- Right curly brace
3. -LRB- Left  round brace
4. -RRB- Right round brace
5. -LSB- Left  square brace
6. -RSB- Right square brace

*/

object TagSet {
  val BRACKETS: Array[(String, String)] = Array(
    ("(", ")"), ("-LRB-", "-RRB-"), // round
    ("{", "}"), ("-LCB-", "-RCB-"), // curly
    ("[", "]"), ("-LSB-", "-RSB-")  // square
  )
}

/*

https://web.stanford.edu/class/cs124/lec/postagging.pdf
https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html

 1. CC   Coordinating conjunction
 2. CD   Cardinal number
 3. DT   Determiner
 4. EX   Existential there
 5. FW   Foreign word
 6. IN   Preposition or subordinating conjunction
 7. JJ   Adjective
 8. JJR  Adjective, comparative
 9. JJS  Adjective, superlative
10. LS   List item marker
11. MD   Modal
12. NN   Noun, singular or mass
13. NNS  Noun, plural
14. NNP  Proper noun, singular
15. NNPS Proper noun, plural
16. PDT  PredeterminerV
17. POS  Possessive ending
18. PRP  Personal pronoun
19. PRP$ Possessive pronoun
20. RB   Adverb
21. RBR  Adverb, comparative
22. RBS  Adverb, superlative
23. RP   Particle
24. SYM  Symbol
25. TO   to
26. UH   Interjection
27. VB   Verb, base form
28. VBD  Verb, past tense
29. VBG  Verb, gerund or present participle
30. VBN  Verb, past participle
31. VBP  Verb, non-3rd person singular present
32. VBZ  Verb, 3rd person singular present
33. WDT  Wh-determiner
34. WP   Wh-pronoun
35. WP$  Possessive wh-pronoun
36. WRB  Wh-adverb

*/

class EnglishTagSet extends TagSet {
  // From RuleBasedEntityFinder.scala.
  // (tags(entity.end).startsWith("N") || tags(entity.end).startsWith("PROPN"))
  // (tags(i).startsWith("N") || tags(i).startsWith("PROPN")
  // From StopwordManager.scala.
  // if (!tags.exists(_.startsWith("NN"))) return false
  def isAnyNoun(tag: String): Boolean = tag.startsWith("N") // It could be NN, but N is faster.

  // From RuleBasedEntityFinder.scala
  // tags(i).startsWith("V"))
  def isAnyVerb(tag: String): Boolean = tag.startsWith("V") // Could be VB, but V is faster.

  // From EntityConstraints.scala
  // if (i > 0 && tag(i - 1).startsWith("JJ"))
  // Non-English versions have been added.
  // From RuleBasedEntityFinder.scala
  // (entity.tags.get.last.startsWith("JJ") || entity.tags.get.last.startsWith("ADJ"))
  def isAnyAdjective(tag: String): Boolean = tag.startsWith("J")  // It could be JJ, but J is faster.

  // From EntityConstraints.scala
  // val coordPOS = Set("CC", ",", "-LRB-", "-RRB-")
  val coordinating: Array[String] = Array("CC", ",", "-LRB-", "-RRB-")
  def isCoordinating(tag: String): Boolean = coordinating.contains(tag) // This will use ==, not startsWith.

  // From EntityConstraints.scala
  // val VALID_FINAL_TAG = """^(DT|N|PROPN|V|JJ|ADJ|ADV|\-R[SR]B).*"""
  // Apparently -RC doesn't cut it.
  // For some reason, RB was not included.  Perhaps English doesn't need it.
  val validFinal: Array[String] = Array("N", "V", "J", /*"RB",*/ "DT", "-RS", "-RR")
  def isValidFinal(tag: String): Boolean = validFinal.exists(tag.startsWith)

  // From StopwordManager.scala
  // isContentPOS(tags(i)) &&
  // tags(i) != "VBN" && // we don't want entities/concepts which consist ONLY of a VBN
  // !StopwordManager.STOP_POS.contains(tags(i)) &&
  // val CONTENT_POS_PREFIXES: Set[String] = Set("ADJ", "NOUN", "NN", "PROPN", "VERB", "VB", "JJ")
  // val STOP_POS: Set[String] = Set("CD")
  // VBN and CD are ruled out.  CD isn't in the list to start with.
  // VBN has a prefix of V and that's why it is an exception.
  val stopwordContent: Array[String] = Array("N", "V", "J")
  def isStopwordContent(tag: String): Boolean = stopwordContent.exists(tag.startsWith(_) && tag !="VBN")

  // From OntologyMapper.scala
  // s.startsWith("N") || s.startsWith("V") || s.startsWith("J")
  // Form Canonicalizer.scala
  // tag.startsWith("NN") || tag.startsWith("VB") || tag.startsWith("JJ")
  val ontologyContentEnglish: Array[String] = Array("N", "V", "J")
  def isOntologyContent(tag: String): Boolean = ontologyContentEnglish.exists(tag.startsWith)

  // From EntityHelper.scala
  // Set of tags that we don't want to begin or end an entity
  //"^PRP".r,
  //"^IN".r,
  //"^TO".r,
  //"^DT".r,
  //",".r,
  // PORTUGUESE
  //"PRON".r,
  ////"ADP".r,
  //"DET".r
  // Set of tags that we don't want to begin or end an entity
  val invalidEdge: Array[String] = Array("PRP", "IN", "TO", "DT", ",")
  def isInvalidEdge(tag: String): Boolean = invalidEdge.exists(tag.startsWith)
}

/*

George said,

VERBF - finite
VERBI - infinite
VERBG - gerund
VERBP - participle, passive

And I take what I said above back. This seems to be a map from multiple types of verbs into these 4. Do you need a full map?

The plain old verb <VERB> goes away.

---and---

This is the conversion table from the tagset we used for Portuguese:

https://universaldependencies.org/tagset-conversion/pt-conll-uposf.html

If I remember correctly, we used the universal tagset enhanced with 4 types of verbs that we derived from the universal dataset itself. Let me know if I need to describe this enhancement in more detail.

---so the tagset is therefer---

ADJ   Adjective
ADP   Preposition?
ADV   Adverb
CCONJ Coordinating conjunction
DET   Determiner
INTJ  Interjection
NOUN  Noun
NUM   Number
PART  Participle?
PRON  Pronoun
PROPN Proper Noun
PUNCT Punctuation
SCONJ Subordinating conjunction
VERBF Verb, finite
VERBG Verb, gerund
VERBI Verb, infinite
VERBP Verb, participle, passive

*/

class PortugueseTagSet extends TagSet {
  // From RuleBasedEntityFinder.scala.
  // (tags(entity.end).startsWith("N") || tags(entity.end).startsWith("PROPN"))
  // (tags(i).startsWith("N") || tags(i).startsWith("PROPN")
  // From StopwordManager.scala.
  // if (!tags.exists(_.startsWith("NN"))) return false
  def isAnyNoun(tag: String): Boolean = tag == "NOUN" || tag == "PROPN"

  // From RuleBasedEntityFinder.scala
  // tags(i).startsWith("V"))
  def isAnyVerb(tag: String): Boolean = tag.startsWith("V")

  // From EntityConstraints.scala
  // if (i > 0 && tag(i - 1).startsWith("JJ"))
  // Non-English versions have been added.
  // From RuleBasedEntityFinder.scala
  // (entity.tags.get.last.startsWith("JJ") || entity.tags.get.last.startsWith("ADJ"))
  def isAnyAdjective(tag: String): Boolean = tag == "ADJ"

  // From EntityConstraints.scala
  // val coordPOS = Set("CC", ",", "-LRB-", "-RRB-")
  val coordinating: Array[String] = Array("CCONJ", ",", "-LRB-", "-RRB-")
  def isCoordinating(tag: String): Boolean = coordinating.contains(tag) // This will use ==, not startsWith.

  // From EntityConstraints.scala
  // val VALID_FINAL_TAG = """^(DT|N|PROPN|V|JJ|ADJ|ADV|\-R[SR]B).*"""
  // Apparently -RC doesn't cut it.
  // For some reason, RB was not included.  Perhaps English doesn't need it.
  val validFinal: Array[String] = Array("N", "PROPN", "V", "ADJ", "ADV", /*"DET",*/ "-RS", "-RR")
  def isValidFinal(tag: String): Boolean = validFinal.exists(tag.startsWith)

  // From StopwordManager.scala
  // isContentPOS(tags(i)) &&
  // tags(i) != "VBN" && // we don't want entities/concepts which consist ONLY of a VBN
  // !StopwordManager.STOP_POS.contains(tags(i)) &&
  // val CONTENT_POS_PREFIXES: Set[String] = Set("ADJ", "NOUN", "NN", "PROPN", "VERB", "VB", "JJ")
  // val STOP_POS: Set[String] = Set("CD")
  // VBN and CD are ruled out.  CD isn't in the list to start with.
  // VBN has a prefix of V and that's why it is an exception.
  val stopwordContent: Array[String] = Array("N", "PROPN", "V", "ADJ")
  def isStopwordContent(tag: String): Boolean = stopwordContent.exists(tag.startsWith)

  // From OntologyMapper.scala
  // s.startsWith("N") || s.startsWith("V") || s.startsWith("J")
  // Form Canonicalizer.scala
  // tag.startsWith("NN") || tag.startsWith("VB") || tag.startsWith("JJ")
  val ontologyContent: Array[String] = Array("N", "PROPN", "V", "ADJ")
  def isOntologyContent(tag: String): Boolean = ontologyContent.exists(tag.startsWith)

  // From EntityHelper.scala
  // Set of tags that we don't want to begin or end an entity
  //"^PRP".r,
  //"^IN".r,
  //"^TO".r,
  //"^DT".r,
  //",".r,
  // PORTUGUESE
  //"PRON".r,
  ////"ADP".r,
  //"DET".r
  // Set of tags that we don't want to begin or end an entity
  val invalidEdge: Array[String] = Array("PRON", /*"ADP",*/ /*"IN",*/ "DET", ",")
  def isInvalidEdge(tag: String): Boolean = invalidEdge.exists(tag.startsWith)
}

class SpanishTagSet extends TagSet {
  val portugueseTagSet = new PortugueseTagSet()

  def isAnyNoun(tag: String): Boolean = portugueseTagSet.isAnyNoun(tag)
  def isAnyVerb(tag: String): Boolean = portugueseTagSet.isAnyVerb(tag)
  def isAnyAdjective(tag: String): Boolean = portugueseTagSet.isAnyAdjective(tag)

  def isCoordinating(tag: String): Boolean = portugueseTagSet.isCoordinating(tag)
  def isValidFinal(tag: String): Boolean = portugueseTagSet.isValidFinal(tag)
  def isStopwordContent(tag: String): Boolean = portugueseTagSet.isStopwordContent(tag)
  def isOntologyContent(tag: String): Boolean = portugueseTagSet.isOntologyContent(tag)
  def isInvalidEdge(tag: String): Boolean = portugueseTagSet.isInvalidEdge(tag)
}