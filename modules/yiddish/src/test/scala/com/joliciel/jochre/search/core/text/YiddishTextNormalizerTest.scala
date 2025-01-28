package com.joliciel.jochre.search.core.text

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.Locale

class YiddishTextNormalizerTest extends AnyFlatSpec with Matchers {
  "A yiddish text normalizer" should "normalize text" in {
    val textNormalizer = TextNormalizer(Locale.forLanguageTag("yi"))
    textNormalizer.normalize("אָ") shouldEqual "א"
  }
}
