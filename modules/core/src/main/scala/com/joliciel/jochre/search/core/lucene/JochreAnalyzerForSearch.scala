package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.lucene.tokenizer.RegexTokenizerFilter
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.core.WhitespaceTokenizer
import org.apache.lucene.analysis.{Analyzer, TokenStream}

import java.util.Locale

case class JochreAnalyzerForSearch(locale: Locale) extends Analyzer {
  override def createComponents(fieldName: String): Analyzer.TokenStreamComponents = {
    val source = new WhitespaceTokenizer();

    new TokenStreamComponents(source, finalFilter(source))
  }

  def finalFilter(tokens: TokenStream): TokenStream = (splitByRegexFilter(_))
    .apply(tokens)

  def splitByRegexFilter(tokens: TokenStream): TokenStream = RegexTokenizerFilter(tokens, locale)
}
