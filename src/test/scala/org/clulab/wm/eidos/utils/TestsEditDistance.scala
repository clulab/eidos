package org.clulab.wm.eidos.utils

// This is the local implementation.
import org.clulab.wm.eidoscommon.utils.{MED => ClulabEditDistance}
import org.clulab.wm.eidoscommon.utils.Test

// These implementations are already "included" in eidos.
// The one to pick might depend on the license.

// This one caches results in a weak map, unfortunately.
import com.sun.xml.bind.v2.util.{EditDistance => SunEditDistance}
import edu.stanford.nlp.util.{EditDistance => StanfordEditDistance}
import org.apache.commons.text.similarity.{LevenshteinDistance => ApacheEditDistance}

abstract class EditDist(source: String, target: String) {
  def measure(): Int
  def transposes: Boolean = false
  def substitutes: Boolean = true
}

class ClulabEditDist(source: String, target: String, override val substitutes: Boolean = true) extends EditDist(source, target) {
  def measure(): Int = ClulabEditDistance(source, target, allowSubstitute = substitutes).getDistance
}

class SunEditDist(source: String, target: String) extends EditDist(source, target) {
  def measure(): Int = SunEditDistance.editDistance(source, target)
}

class StanfordEditDist(source: String, target: String, override val transposes: Boolean = false) extends EditDist(source, target) {
  def measure(): Int = new StanfordEditDistance(/*allowTranspose =*/ transposes).score(source, target).toInt
}

class ApacheEditDist(source: String, target: String) extends EditDist(source, target) {
  def measure(): Int = new ApacheEditDistance().apply(source, target)
}

class TestsEditDistance extends Test {

  def test(name: String, constructor: (String, String) => EditDist): Unit = {
    behavior of name

    it should "insert correctly" in {
      val med = constructor("ac", "abc")

      // depends on if can substitute and transpose
      med.measure should be (1)
    }

    it should "delete correctly" in {
      val med = constructor("abc", "ac")

      med.measure should be (1)
    }

    it should "substitute correctly" in {
      val med = constructor("abc", "adc")
      val expected = if (med.substitutes) 1 else 2

      med.measure should be (expected)
    }

    it should "transpose correctly" in {
      val med = constructor("ab", "ba")
      val expected = if (med.transposes) 1 else 2

      med.measure should be (expected)
    }
  }

  test("SunEditDistance", (source: String, target: String) => new SunEditDist(source, target))
  test("ApacheEditDistance", (source: String, target: String) => new ApacheEditDist(source, target))
  test("StanfordEditDistance, non-transposing", (source: String, target: String) => new StanfordEditDist(source, target, transposes = false))
  test("StanfordEditDistance, transposing", (source: String, target: String) => new StanfordEditDist(source, target, transposes = true))
  test ("ClulabEditDistance, non-substituting", (source: String, target: String) => new ClulabEditDist(source, target, substitutes = false))
  test ("ClulabEditDistance, substituting", (source: String, target: String) => new ClulabEditDist(source, target, substitutes = true))
}
