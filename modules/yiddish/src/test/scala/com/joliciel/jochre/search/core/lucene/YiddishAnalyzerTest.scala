package com.joliciel.jochre.search.core.lucene

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.joliciel.jochre.search.yiddish.YiddishFilters
import com.joliciel.jochre.search.core.SearchCriterion
import com.joliciel.jochre.search.core.IndexField

class YiddishAnalyzerTest extends AnyFlatSpec with Matchers with LuceneUtilities {
  "A Yiddish index field analyzer" should "correctly handle Latin alphabet text" in {
    val analyzerGroup = AnalyzerGroup.generic(Some(YiddishFilters))
    val analyzer = analyzerGroup.forIndexingUntokenizedFields

    val text = "Sholem Aleichem, 1859-1916"

    val tokens = tokenizeString(text, analyzer)

    val expected = Seq(
      "sholem aleichem, 1859-1916"
    )

    tokens.map(_.value) should equal(expected)
  }

  "A Yiddish Contains Criterion" should "correctly handle different double-quote characters" in {
    val analyzerGroup = AnalyzerGroup.generic(Some(YiddishFilters))

    val queryWithAsciiQuotes = "\"פאר דער\""
    val queryWithUnicodeQuotes = "״פאר דער״"

    val unicodeCriterion = SearchCriterion.Contains(IndexField.Text, queryWithUnicodeQuotes, true)
    val unicodeQuery = unicodeCriterion.toLuceneQuery(analyzerGroup)

    val asciiCriterion = SearchCriterion.Contains(IndexField.Text, queryWithAsciiQuotes, true)
    val asciiQuery = asciiCriterion.toLuceneQuery(analyzerGroup)

    asciiQuery.toString() shouldEqual unicodeQuery.toString()
  }

}
