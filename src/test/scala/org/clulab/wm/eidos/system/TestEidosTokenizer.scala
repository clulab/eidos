package org.clulab.wm.eidos.system

import org.clulab.processors.clu.tokenizer.RawToken
import org.clulab.wm.eidos.test.EnglishTest
import org.clulab.wm.eidoscommon.{EidosEnglishProcessor, EidosTokenizer}

class TestEidosTokenizer extends EnglishTest {

  behavior of "normalization"

  val eidosTokenizer: EidosTokenizer = ieSystem.components.procOpt.get.asInstanceOf[EidosEnglishProcessor].eidosTokenizer

  it should "not change plain text" in {
    val oldText = "This is a test."
    val newText = eidosTokenizer.normalize(oldText)._1

    newText should be (oldText)
  }

  it should "not normalize reversed diacritics" in {
    val oldText = "Universit\u00a8at Koblenz-Landau"
    val newText = eidosTokenizer.normalize(oldText)._1

    newText should be ("Universit \u0308at Koblenz-Landau")
  }

  it should "work OK with UTF-32 characters" in {
    val oldText = "(\ud83d\udca9)"
    val normalizedText = eidosTokenizer.normalize(oldText)._1
    val sanitizedText = eidosTokenizer.sanitize(oldText, keepAccents = true)._1

    normalizedText should be (oldText)
    sanitizedText should be ("(  )")
  }

  it should "normalize abnormal text" in {
    val inputsAndExpecteds = Seq(
      ("e\ufb00ort", "effort"),
      ("a\ufb01eld", "afield"),
      ("e\ufb03cient", "efficient"), // (ffi)
      ("e\ufb00icient", "efficient"), // (ff)i
      ("ef\ufb01cient", "efficient"), // f(fi)
    )

    inputsAndExpecteds.foreach { case (input, expected) =>
      val actual = eidosTokenizer.normalize(input)._1

      actual should be (expected)
    }
  }

  it should "make unicode substitutions" in {
    val inputsAndExpecteds = Seq(
      // (input, keepAccents = false, keepAccents = true)
      ("(alpha)",  "(alpha)", "(alpha)"),
      ("(\u0277)", "(omega)", "(omega)"),
      ("(\u0103)", "(a)",      "(a)"),      // a-umlaut, in unicode list, not in accent list
      ("(\u00e1)", "(a)",      "(\u00e1)"), // a-acute, in unicode list, in accent list
      ("(\u017f)", "( )",      "( )"),      // long s, not in unicode list, not in accent list
      ("(\u00b8)", "()",       "()")        // unknown, in unicode list empty, not in accent list
    )

    inputsAndExpecteds.foreach { case (input, expectedFalse, expectedTrue) =>
      val actualFalse = eidosTokenizer.sanitize(input, keepAccents = false)._1
      val actualTrue =  eidosTokenizer.sanitize(input, keepAccents = true)._1

      actualFalse should be (expectedFalse)
      actualTrue should be (expectedTrue)
    }
  }

  it should "tokenize simple unicode corrrectly" in {
    val text = "(\u0277)"
    val tokens = eidosTokenizer.entoken(text)

    tokens should have size 4
    tokens(0) should be (RawToken("(", 0, 1, "("))
    tokens(1) should be (RawToken("\u0277", 1, 2, "omega"))
    tokens(2) should be (RawToken(")", 2, 3, ")"))
    tokens(3) should be (RawToken("", 3, 3, ".")) // The paragraph splitter ands a trailing period.
  }

  it should "tokenize complicated unicode corrrectly" in {
    // First to be normalized to a th, the second to be deleted, third to be inserted,
    // and the fourth is to be replaced by a space so that the result is
    // "(", "||omega", ")"
    val text = "(\u00fe|\u00b8|\u0277|\u00a8)"
    val tokens = eidosTokenizer.entoken(text)

    tokens should have size 8
    tokens(0) should be (RawToken("(", 0, 1, "("))
    tokens(1) should be (RawToken("\u00fe", 1, 2, "th"))
    tokens(2) should be (RawToken("|", 2, 3, "|"))
    tokens(3) should be (RawToken("|", 4, 5, "|"))
    tokens(4) should be (RawToken("\u0277", 5, 6, "omega"))
    tokens(5) should be (RawToken("|", 6, 7, "|"))
    tokens(6) should be (RawToken(")", 8, 9, ")"))
    tokens(7) should be (RawToken("", 9, 9, ".")) // The paragraph splitter ands a trailing period.
  }

  it should "handle problematic text correctly" in {
    {
      // This comes from our gold_groundings_annotated.tsv.
      // It started out as UTF-8 \xe8 \x98 \x91 and converts to UTF-16 as \u8611.
      val text = "\u8611 Restoring the severely damaged environment."
      val expectedWords = Seq("Restoring", "the", "severely", "damaged", "environment", ".")
      val actualWords = eidosTokenizer.entoken(text).map(_.word)

      expectedWords should contain theSameElementsAs actualWords
    }

    {
      // This comes from our gold_graoundings.tsv.
      // It started out as UTF-8 \xc4 \xa2 after some bad conversion from the text above.
      val text = "\uc4a2 Restoring the severely damaged environment."
      val expectedWords = Seq("Restoring", "the", "severely", "damaged", "environment", ".")
      val actualWords = eidosTokenizer.entoken(text).map(_.word)

      expectedWords should contain theSameElementsAs actualWords
    }
  }

  it should "work end to end" in {
    val text = "It runs the gamut from \u03b1 to \u0277."
    val sentences = eidosTokenizer.tokenize(text)

    val expectedTokens = Seq("It", "runs", "the", "gamut", "from", "\u03b1", "to", "\u0277", ".")
    val actualTokens = sentences.flatMap { sentence => sentence.raw }

    expectedTokens should contain theSameElementsAs actualTokens

    val expectedWords = Seq("It", "runs", "the", "gamut", "from", "alpha", "to", "omega", ".")
    val actualWords = sentences.flatMap { sentence => sentence.words }

    expectedWords should contain theSameElementsAs actualWords
  }
}
