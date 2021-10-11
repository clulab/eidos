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

class ClulabEditDist(source: String, target: String, override val substitutes: Boolean = true, override val transposes: Boolean = false) extends EditDist(source, target) {
  def measure(): Int = ClulabEditDistance(source, target, allowSubstitute = substitutes, allowTranspose = transposes).getDistance
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
      val expected = (med.substitutes, med.transposes) match {
        case (false, false) => 1
        case (false, true) => 1
        case (true, false) => 1
        case (true, true) => 1
      }

      med.measure should be (expected)
    }

    it should "delete correctly" in {
      val med = constructor("abc", "ac")
      val expected = (med.substitutes, med.transposes) match {
        case (false, false) => 1
        case (false, true) => 1
        case (true, false) => 1
        case (true, true) => 1
      }

      med.measure should be (expected)
    }

    it should "substitute correctly" in {
      val med = constructor("abc", "adc")
      val expected = (med.substitutes, med.transposes) match {
        case (false, false) => 2
        case (false, true) => 2
        case (true, false) => 1
        case (true, true) => 1
      }

      med.measure should be (expected)
    }

    it should "transpose correctly" in {
      val med = constructor("AabB", "AbaB")
      val expected = (med.substitutes, med.transposes) match {
        case (false, false) => 2
        case (false, true) => 1
        case (true, false) => 2
        case (true, true) => 1
      }

      med.measure should be (expected)
    }
  }

  test("SunEditDistance", (source: String, target: String) => new SunEditDist(source, target))
  test("ApacheEditDistance", (source: String, target: String) => new ApacheEditDist(source, target))
  test("StanfordEditDistance, non-transposing", (source: String, target: String) => new StanfordEditDist(source, target, transposes = false))
  test("StanfordEditDistance, transposing", (source: String, target: String) => new StanfordEditDist(source, target, transposes = true))
  test ("ClulabEditDistance, non-substituting, non-transposing", (source: String, target: String) => new ClulabEditDist(source, target, substitutes = false, transposes = false))
  test ("ClulabEditDistance, non-substituting, transposing", (source: String, target: String) => new ClulabEditDist(source, target, substitutes = false, transposes = true))
  test ("ClulabEditDistance, substituting, non-transposing", (source: String, target: String) => new ClulabEditDist(source, target, substitutes = true, transposes = false))
  test ("ClulabEditDistance, substituting, transposing",(source: String, target: String) => new ClulabEditDist(source, target, substitutes = true, transposes = true))

  // TODO, run a bunch of them and make sure gets the same answer


}
