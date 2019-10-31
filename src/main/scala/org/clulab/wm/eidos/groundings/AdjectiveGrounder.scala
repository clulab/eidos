package org.clulab.wm.eidos.groundings

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.wm.eidos.Aliases.Quantifier
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sourcer

case class AdjectiveGrounding(intercept: Option[Double], mu: Option[Double], sigma: Option[Double]) {
  protected def isGrounded: Boolean = intercept.isDefined && mu.isDefined && sigma.isDefined

  def predictDelta(mean: Double, stdev: Double): Option[Double] =
      if (!isGrounded) None
      else Some(math.pow(math.E, intercept.get + (mu.get * mean) + (sigma.get * stdev)) * stdev)
}

object AdjectiveGrounding {
  val noAdjectiveGrounding = AdjectiveGrounding(None, None, None)
}

trait AdjectiveGrounder {
  def groundAdjective(adjective: String): AdjectiveGrounding
}

class  EidosAdjectiveGrounder(quantifierKBFile: String) extends AdjectiveGrounder {

  protected def load(): Map[Quantifier, Map[String, Double]] =
      Sourcer.sourceFromResource(quantifierKBFile).autoClose { source =>
      // adjective -> Map(name:value)
        FileUtils.getCommentedLinesFromSource(source)
            .map { line => // "adjective  mu_coefficient  sigma_coefficient  intercept"
              val fields = line.split("\t")
              val adj = fields(0)
              val mu_coeff = fields(1).toDouble
              val sigma_coeff = fields(2).toDouble
              val intercept = fields(3).toDouble
              adj -> Map(EidosAdjectiveGrounder.MU_COEFF -> mu_coeff, EidosAdjectiveGrounder.SIGMA_COEFF -> sigma_coeff, EidosAdjectiveGrounder.INTERCEPT -> intercept)
            }.toMap
      }
  
  protected val grounder: Map[Quantifier, Map[String, Double]] = load()
  
  def groundAdjective(adjective: String): AdjectiveGrounding = {
    
    def stemIfAdverb(word: String) = {
      if (word.endsWith("ly"))
        if (word.endsWith("ily"))
          word.slice(0, word.length - 3) ++ "y"
        else
          word.slice(0, word.length - 2)
      else
        word
    }
    
    val pseudoStemmed = stemIfAdverb(adjective)
    val modelRow = grounder.getOrElse(pseudoStemmed, Map.empty)
    
    if (modelRow.isEmpty)
      AdjectiveGrounding.noAdjectiveGrounding
    else {
      val intercept = modelRow.get(EidosAdjectiveGrounder.INTERCEPT)
      val mu = modelRow.get(EidosAdjectiveGrounder.MU_COEFF)
      val sigma = modelRow.get(EidosAdjectiveGrounder.SIGMA_COEFF)
      
      AdjectiveGrounding(intercept, mu, sigma)
    }
  }
}

object EidosAdjectiveGrounder {
  val INTERCEPT: String = "intercept"
  val MU_COEFF: String = "mu_coeff"
  val SIGMA_COEFF: String = "sigma_coeff"

  def apply(quantifierKBFile: String): EidosAdjectiveGrounder = new EidosAdjectiveGrounder(quantifierKBFile)

  def fromConfig(config: Config): EidosAdjectiveGrounder = apply(config.getString("quantifierKBPath"))

  def fromEidosConfig(config: Config): EidosAdjectiveGrounder = fromConfig(config[Config]("adjectiveGrounder"))
}
