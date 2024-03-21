package com.joliciel.jochre.search.core.lucene.tokenizer

import com.joliciel.jochre.search.core.lucene.LuceneUtilities
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.core.WhitespaceTokenizer
import org.apache.lucene.analysis.{Analyzer, TokenStream}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.matching.Regex

class RegexTokenizerFilterTest extends AnyFlatSpec with Matchers with LuceneUtilities {
  class TestAnalyzer(regex: Regex) extends Analyzer {
    override def createComponents(fieldName: String): Analyzer.TokenStreamComponents = {
      val source = new WhitespaceTokenizer();

      new TokenStreamComponents(source, finalFilter(source))
    }

    def finalFilter(tokens: TokenStream): TokenStream = {
      new RegexTokenizerFilter(tokens, regex)
    }
  }

  "A RegexTokenizerFilter" should "correctly analyze a sentence" in {
    val regex: Regex = raw"(?Ui)(\baujourd'hui|\bj'|\bn'|'s\b|\b\w+n't\b|\p{Punct}+)".r
    val analyzer: Analyzer = new TestAnalyzer(regex)

    val text = "\"Aujourd'hui, j'avais du temps. N'avais... aujourd'hui rien à faire. Bob's dog's's shouldn't bark.\""
    val tokens = tokenizeString(text, analyzer)

    val expected = Seq(
      "\"",
      "Aujourd'hui",
      ",",
      "j'",
      "avais",
      "du",
      "temps",
      ".",
      "N'",
      "avais",
      "...",
      "aujourd'hui",
      "rien",
      "à",
      "faire",
      ".",
      "Bob",
      "'s",
      "dog",
      "'s",
      "'s",
      "shouldn't",
      "bark",
      ".\""
    )

    tokens.map(_.value) should equal(expected)
  }
}
