package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.text.LanguageSpecificFilters
import com.joliciel.jochre.search.core.util.AndThenIf.Implicits._
import org.apache.lucene.analysis.TokenStream
import org.slf4j.LoggerFactory

import java.util.Locale

class JochreAnalyzerForIndexFields(locale: Locale,
  languageSpecificFilters: Option[LanguageSpecificFilters] = None) extends JochreAnalyzerBase(locale) {
  private val log = LoggerFactory.getLogger(getClass)

  val indexingHelper = IndexingHelper()

  private val maybePostTokenizationFilter = languageSpecificFilters.flatMap(_.postTokenizationFilterForIndex)
  private def postTokenizationFilter(tokens: TokenStream): TokenStream = {
    maybePostTokenizationFilter.map(_(tokens)).getOrElse(tokens)
  }

  override def finalFilter(tokens: TokenStream): TokenStream = (textNormalizingFilter(_))
    .andThen(regexTokenizerFilter)
    .andThen(lowercaseFilter)
    .andThen(postTokenizationFilter)
    .andThen(ignorePunctuationFilter)
    .andThenIf(log.isTraceEnabled)(tapFilter(log, "final"))
    .apply(tokens)
}
