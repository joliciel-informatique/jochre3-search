package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.lucene.tokenizer.{
  IgnorePunctuationFilter,
  RegexTokenizerFilter,
  SkipPunctuationFilter,
  TapFilter,
  TextNormalizingFilter
}
import com.joliciel.jochre.search.core.text.TextNormalizer
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.core.WhitespaceTokenizer
import org.apache.lucene.analysis.{Analyzer, LowerCaseFilter, TokenStream}
import org.apache.lucene.analysis.icu.ICUFoldingFilter
import org.slf4j.Logger

import java.util.Locale
import org.apache.lucene.analysis.miscellaneous.RemoveDuplicatesTokenFilter
import org.apache.lucene.analysis.Tokenizer

private[lucene] abstract case class JochreAnalyzerBase(locale: Locale) extends Analyzer {
  val whitespaceTokenizer = new WhitespaceTokenizer()
  def baseTokenizer: Tokenizer = whitespaceTokenizer

  override def createComponents(fieldName: String): Analyzer.TokenStreamComponents = {
    val source = baseTokenizer

    new TokenStreamComponents(source, finalFilter(source))
  }

  def finalFilter(tokens: TokenStream): TokenStream

  protected def regexTokenizerFilter(tokens: TokenStream): TokenStream = RegexTokenizerFilter(tokens, locale)
  protected def tapFilter(log: Logger, logName: String = "")(tokens: TokenStream): TokenStream =
    new TapFilter(tokens, log, logName)

  protected def lowercaseFilter(tokens: TokenStream): TokenStream = new LowerCaseFilter(tokens)
  protected def icuFoldingFilter(tokens: TokenStream): TokenStream = new ICUFoldingFilter(tokens)
  protected def skipPunctuationFilter(tokens: TokenStream): TokenStream = new SkipPunctuationFilter(tokens)
  protected def ignorePunctuationFilter(tokens: TokenStream): TokenStream = new IgnorePunctuationFilter(tokens)
  protected def textNormalizingFilter(tokens: TokenStream): TokenStream =
    new TextNormalizingFilter(tokens, TextNormalizer(locale))

  protected def removeDuplicatesFilter(tokens: TokenStream): TokenStream =
    new RemoveDuplicatesTokenFilter(tokens)
}
