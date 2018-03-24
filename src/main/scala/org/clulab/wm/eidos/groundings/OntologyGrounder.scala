package org.clulab.wm.eidos.groundings

import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.odin.Mention
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.FileUtils

case class OntologyGrounding(grounding: Seq[(String, Double)])

trait OntologyGrounder {
  def groundOntology(mention: EidosMention): OntologyGrounding
  def containsStopword(stopword: String): Boolean
}

class EidosOntologyGrounder(stopwordsPath: String, transparentPath: String) {
  protected val stopwords = FileUtils.getCommentedTextsFromResource(stopwordsPath).toSet
  protected val transparentWords = FileUtils.getCommentedTextsFromResource(transparentPath).toSet
  protected val bothWords = stopwords ++ transparentWords
  
  // Be careful, because object may not be completely constructed.
  def groundOntology(mention: EidosMention, wordToVec: EidosWordToVec): OntologyGrounding = {
    if (mention.odinMention.matches("Entity")) { // TODO: Store this string somewhere
      val canonicalName = mention.canonicalName
      // Make vector for canonicalName
      val canonicalNameParts = canonicalName.split(" +")

      OntologyGrounding(wordToVec.calculateSimilarities(canonicalNameParts))
    }
    else
      OntologyGrounding(Seq.empty)
  }
  
  def containsStopword(stopword: String): Boolean = bothWords.contains(stopword)
  
  protected def hasContent(m: Mention): Boolean = {
    // TODO: make this exists
    val contentfulLemmas = m.lemmas.get.filterNot(lemma => bothWords.contains(lemma)) 
    
    contentfulLemmas.nonEmpty 
  }   
  
  def filterStopTransparent(mentions: Seq[Mention]): Seq[Mention] =
      // Remove mentions which are entirely stop/transparent words 
      mentions.filter(hasContent) 
}

object EidosOntologyGrounder {
  
  def apply(stopWordsPath: String, transparentPath: String) = new EidosOntologyGrounder(stopWordsPath, transparentPath)
}
