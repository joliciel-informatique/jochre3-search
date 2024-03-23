package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.lucene.tokenizer.{RegexTokenizerFilter, TapFilter}
import org.apache.lucene.analysis.{Analyzer, LowerCaseFilter, TokenStream}
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.core.WhitespaceTokenizer
import org.slf4j.Logger

import java.util.Locale

private[lucene] abstract case class JochreAnalyzerBase (locale: Locale) extends Analyzer {
  override def createComponents(fieldName: String): Analyzer.TokenStreamComponents = {
    val source = new WhitespaceTokenizer();

    new TokenStreamComponents(source, finalFilter(source))
  }

  def finalFilter(tokens: TokenStream): TokenStream

  protected def regexTokenizerFilter(tokens: TokenStream): TokenStream = RegexTokenizerFilter(tokens, locale)
  protected def tapFilter(log: Logger, logName: String = "")(tokens: TokenStream): TokenStream =
    new TapFilter(tokens, log, logName)

  protected def lowercaseFilter(tokens: TokenStream): TokenStream = new LowerCaseFilter(tokens)
}
