package com.joliciel.jochre.search.core.lucene

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.joliciel.jochre.search.yiddish.YiddishFilters

class YiddishAnalyzerTest extends AnyFlatSpec with Matchers with LuceneUtilities {
  "A Yiddish index field analyzer" should "transform hyphens" in {
    val analyzerGroup = AnalyzerGroup.generic(Some(YiddishFilters))
    val analyzer = analyzerGroup.forIndexingUntokenizedFields

    val text = "Sholem Aleichem, 1859-1916"

    val tokens = tokenizeString(text, analyzer)

    val expected = Seq(
      "sholem aleichem, 1859-1916"
    )

    tokens.map(_.value) should equal(expected)
  }

}
