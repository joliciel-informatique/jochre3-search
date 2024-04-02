package com.joliciel.jochre.search.core.lucene.tokenizer

import com.joliciel.jochre.search.core.lucene.LuceneUtilities
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.core.WhitespaceTokenizer
import org.apache.lucene.analysis.{Analyzer, TokenStream}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkipWildcardInSearchFilterTest extends AnyFlatSpec with Matchers with LuceneUtilities {
  class TestAnalyzer() extends Analyzer {
    override def createComponents(fieldName: String): Analyzer.TokenStreamComponents = {
      val source = new WhitespaceTokenizer();

      new TokenStreamComponents(source, finalFilter(source))
    }

    def finalFilter(tokens: TokenStream): TokenStream = {
      new SkipWildcardInSearchFilter(tokens)
    }
  }

  "SkipWildcardMarkerFilter" should "skip wildcards correctly" in {
    val analyzer: Analyzer = new TestAnalyzer()

    val text = "dog * cat"
    val tokensAndPositions = tokenizeStringWithPositions(text, analyzer)

    val expected = Seq(
      ("dog", 1),
      ("cat", 2)
    )

    tokensAndPositions.map { case (token, position) => (token.value, position) } should equal(expected)
  }

  it should "handle consecutive wildcards correctly" in {
    val analyzer: Analyzer = new TestAnalyzer()

    val text = "dog * * cat"
    val tokensAndPositions = tokenizeStringWithPositions(text, analyzer)

    val expected = Seq(
      ("dog", 1),
      ("cat", 3)
    )

    tokensAndPositions.map { case (token, position) => (token.value, position) } should equal(expected)
  }

  it should "handle complex wildcards correctly" in {
    val analyzer: Analyzer = new TestAnalyzer()

    val text = "dog * * cat * mouse"
    val tokensAndPositions = tokenizeStringWithPositions(text, analyzer)

    val expected = Seq(
      ("dog", 1),
      ("cat", 3),
      ("mouse", 2)
    )

    tokensAndPositions.map { case (token, position) => (token.value, position) } should equal(expected)
  }
}
