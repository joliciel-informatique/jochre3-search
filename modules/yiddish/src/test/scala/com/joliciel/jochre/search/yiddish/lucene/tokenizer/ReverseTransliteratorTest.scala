package com.joliciel.jochre.search.yiddish.lucene.tokenizer

import com.joliciel.jochre.search.core.lucene.LuceneUtilities
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.core.WhitespaceTokenizer
import org.apache.lucene.analysis.{Analyzer, TokenStream}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReverseTransliteratorTest extends AnyFlatSpec with Matchers with LuceneUtilities {
  class TestAnalyzer() extends Analyzer {
    override def createComponents(fieldName: String): Analyzer.TokenStreamComponents = {
      val source = new WhitespaceTokenizer();

      new TokenStreamComponents(source, finalFilter(source))
    }

    def finalFilter(tokens: TokenStream): TokenStream = {
      new ReverseTransliterator(tokens)
    }
  }

  "A ReverseTransliterator" should "work" in {
    val analyzer: Analyzer = new TestAnalyzer()

    val text = """nifter"""
    val tokens = tokenizeString(text, analyzer)

    val expected = Seq(
      "ניפטר",
      "nifter"
    )

    tokens.map(_.value) should equal(expected)
  }
}
