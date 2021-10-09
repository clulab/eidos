package org.clulab.wm.eidos.utils

import org.clulab.wm.eidoscommon.utils.MED
import org.clulab.wm.eidoscommon.utils.Test

// These implementations are already "included" in eidos.
// The one to pick might depend on the license.

// This one caches results in a weak map, unfortunately.
import com.sun.xml.bind.v2.util.{EditDistance => SunEditDistance}
import edu.stanford.nlp.util.{EditDistance => StanfordEditDistance}
import org.apache.commons.text.similarity.{LevenshteinDistance => ApacheEditDistance}

class TestsEditDistance extends Test {

  behavior of "SunEditDistance"

  it should "insert correctly" in {
    val med = SunEditDistance.editDistance("ac", "abc")

    med should be (1)
  }

  it should "delete correctly" in {
    val med = SunEditDistance.editDistance("abc", "ac")

    med should be (1)
  }

  it should "substitute correctly" in {
    val med = SunEditDistance.editDistance("abc", "adc")

    med should be (1)
  }

  it should "transpose correctly" in {
    val med = SunEditDistance.editDistance("ab", "ba")

    // This might not be optimal.
    med should be (2)
  }

  behavior of "ApacheEditDistance"

  it should "insert correctly" in {
    val med = new ApacheEditDistance().apply("ac", "abc")

    med should be (1)
  }

  it should "delete correctly" in {
    val med = new ApacheEditDistance().apply("abc", "ac")

    med should be (1)
  }

  it should "substitute correctly" in {
    val med = new ApacheEditDistance().apply("abc", "adc")

    med should be (1)
  }

  it should "transpose correctly" in {
    val med = new ApacheEditDistance().apply("ab", "ba")

    // This might not be optimal.
    med should be (2)
  }

  behavior of "StanfordEditDistance"

  it should "insert correctly" in {
    val med = new StanfordEditDistance().score("ac", "abc")

    med should be (1)
  }

  it should "delete correctly" in {
    val med = new StanfordEditDistance().score("abc", "ac")

    med should be (1)
  }

  it should "substitute correctly" in {
    val med = new StanfordEditDistance().score("abc", "adc")

    med should be (1)
  }

  it should "transpose correctly" in {
    val med = new StanfordEditDistance(false).score("ab", "ba")

    // This might not be optimal.
    med should be (2)
  }

  it should "transpose (in)correctly" in {
    val med = new StanfordEditDistance(true).score("ab", "ba")

    med should be (1)
  }

  behavior of "MED"

  it should "insert correctly" in {
    val med = MED("ac", "abc").getDistance

    med should be (1)
  }

  it should "delete correctly" in {
    val med = MED("abc", "ac").getDistance

    med should be (1)
  }

  it should "substitute correctly" in {
    val med = MED("abc", "adc").getDistance

    med should be (1)
  }

  it should "transpose correctly" in {
    val med = MED("ab", "ba").getDistance

    // This might not be optimal.
    med should be (2)
  }
}
