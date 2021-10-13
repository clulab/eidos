package org.clulab.wm.eidoscommon.utils

class TestMED extends Test {
  behavior of "MED"

  it should "work, substitute = false, transpose = false" in {
    val allowSubstitute = false
    val allowTranspose = false

    MED("cat", "cars", allowSubstitute, allowTranspose).getDistance should be (3)
    MED("kitten", "sitting", allowSubstitute, allowTranspose).getDistance should be (5)
    MED("Sunday", "Saturday", allowSubstitute, allowTranspose).getDistance should be (4)
    MED("meter", "litre", allowSubstitute, allowTranspose).getDistance should be (6)
  }

  it should "work, substitute = true, transpose = false" in {
    val allowSubstitute = true
    val allowTranspose = false

    MED("cat", "cars", allowSubstitute, allowTranspose).getDistance should be (2)
    MED("kitten", "sitting", allowSubstitute, allowTranspose).getDistance should be (3)
    MED("Sunday", "Saturday", allowSubstitute, allowTranspose).getDistance should be (3)
    MED("meter", "litre", allowSubstitute, allowTranspose).getDistance should be (4)
  }

  it should "work, substitute = false, transpose = true" in {
    val allowSubstitute = false
    val allowTranspose = true

    MED("cat", "cars", allowSubstitute, allowTranspose).getDistance should be (3)
    MED("kitten", "sitting", allowSubstitute, allowTranspose).getDistance should be (5)
    MED("Sunday", "Saturday", allowSubstitute, allowTranspose).getDistance should be (4)
    MED("meter", "litre", allowSubstitute, allowTranspose).getDistance should be (5)
  }

  it should "work, substitute = true, transpose = true" in {
    val allowSubstitute = true
    val allowTranspose = true

    MED("cat", "cars", allowSubstitute, allowTranspose).getDistance should be (2)
    MED("kitten", "sitting", allowSubstitute, allowTranspose).getDistance should be (3)
    MED("Sunday", "Saturday", allowSubstitute, allowTranspose).getDistance should be (3)
    MED("meter", "litre", allowSubstitute, allowTranspose).getDistance should be (3)
  }
}
