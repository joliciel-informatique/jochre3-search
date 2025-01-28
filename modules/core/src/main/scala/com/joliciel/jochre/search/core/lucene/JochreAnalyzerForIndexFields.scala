package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.text.LanguageSpecificFilters
import com.joliciel.jochre.search.core.util.AndThenIf.Implicits._
import org.apache.lucene.analysis.{Tokenizer, TokenStream}
import org.slf4j.LoggerFactory

import java.util.Locale
import org.apache.lucene.analysis.core.KeywordTokenizer
import org.apache.lucene.analysis.core.WhitespaceTokenizer

class JochreAnalyzerForIndexFields(
    locale: Locale,
    tokenize: Boolean,
    languageSpecificFilters: Option[LanguageSpecificFilters] = None
) extends JochreAnalyzerBase(locale) {
  private val log = LoggerFactory.getLogger(getClass)

  val indexingHelper = IndexingHelper()

  private val maybePostTokenizationFilter = languageSpecificFilters.flatMap(_.postTokenizationFilterForIndex)
  private def postTokenizationFilter(tokens: TokenStream): TokenStream = {
    maybePostTokenizationFilter.map(_(tokens)).getOrElse(tokens)
  }

  override def baseTokenizer: Tokenizer = if (tokenize) {
    super.baseTokenizer
  } else {
    new KeywordTokenizer()
  }

  override def finalFilter(tokens: TokenStream): TokenStream = (textNormalizingFilter(_))
    .andThenIf(tokenize)(regexTokenizerFilter)
    .andThen(lowercaseFilter)
    .andThen(icuFoldingFilter)
    .andThen(postTokenizationFilter)
    .andThen(ignorePunctuationFilter)
    .andThenIf(log.isTraceEnabled)(tapFilter(log, "final"))
    .apply(tokens)
}
