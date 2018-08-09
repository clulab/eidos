package org.clulab.wm.eidos.groundings

object Word2VecUtils {

  /** Normalizes this vector to length 1, in place */
  def norm(weights:Array[Double]) {
    var i = 0
    var len = 0.0
    while (i < weights.length) {
      len += weights(i) * weights(i)
      i += 1
    }
    len = math.sqrt(len)
    i = 0
    if (len != 0) {
      while (i < weights.length) {
        weights(i) /= len
        i += 1
      }
    }
  }

  def dotProduct(v1:Array[Double], v2:Array[Double]):Double = {
    assert(v1.length == v2.length) //should we always assume that v2 is longer? perhaps set shorter to length of longer...
    var sum = 0.0
    var i = 0
    while(i < v1.length) {
      sum += v1(i) * v2(i)
      i += 1
    }
    sum
  }

  protected def isNumber(w:String):Boolean = {
    var i = 0
    var foundDigit = false
    while(i < w.length) {
      val c = w.charAt(i)
      if(! Character.isDigit(c) &&
        c != '-' && c != '+' &&
        c != ',' && c != '.' &&
        c != '/' && c != '\\')
        return false
      if(Character.isDigit(c))
        foundDigit = true
      i += 1
    }
    foundDigit
  }

  /**
    * Normalizes words for word2vec
    * @param uw A word (NOT lemma)
    * @return The normalized form of the word
    */
  def sanitizeWord(uw:String, keepNumbers:Boolean = true):String = {
    val w = uw.toLowerCase()

    // skip parens from corenlp
    if(w == "-lrb-" || w == "-rrb-" || w == "-lsb-" || w == "-rsb-") {
      return ""
    }

    // skip URLS
    if(w.startsWith("http") || w.contains(".com") || w.contains(".org")) //added .com and .org to cover more urls (becky)
      return ""

    // normalize numbers to a unique token
    if(isNumber(w)) {
      if(keepNumbers) return "xnumx"
      else return ""
    }

    // remove all non-letters; convert letters to lowercase
    val os = new collection.mutable.StringBuilder()
    var i = 0
    while(i < w.length) {
      val c = w.charAt(i)
      // added underscore since it is our delimiter for dependency stuff...
      if(Character.isLetter(c) || c == '_') os += c
      i += 1
    }
    os.toString()
  }
}
