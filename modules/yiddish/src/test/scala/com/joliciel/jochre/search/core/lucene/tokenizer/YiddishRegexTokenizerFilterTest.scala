package com.joliciel.jochre.search.core.lucene.tokenizer

import com.joliciel.jochre.search.core.lucene.LuceneUtilities
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.core.WhitespaceTokenizer
import org.apache.lucene.analysis.{Analyzer, TokenStream}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.Locale

class YiddishRegexTokenizerFilterTest extends AnyFlatSpec with Matchers with LuceneUtilities {
  class TestAnalyzerForLocale(locale: Locale) extends Analyzer {
    override def createComponents(fieldName: String): Analyzer.TokenStreamComponents = {
      val source = new WhitespaceTokenizer();

      new TokenStreamComponents(source, finalFilter(source))
    }

    def finalFilter(tokens: TokenStream): TokenStream = {
      RegexTokenizerFilter(tokens, locale)
    }
  }

  "A RegexTokenizerFilter" should "work in Yiddish" in {
    val analyzer: Analyzer = new TestAnalyzerForLocale(Locale.forLanguageTag("yi"))

    val text = """ס‛האָט"""
    val tokens = tokenizeString(text, analyzer)

    val expected = Seq(
      "ס‛",
      "האָט"
    )

    tokens.map(_.value) should equal(expected)
  }
}
