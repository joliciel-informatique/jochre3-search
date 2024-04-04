package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.util.AndThenIf.Implicits._
import org.apache.lucene.analysis.TokenStream
import org.slf4j.LoggerFactory

import java.util.Locale

class JochreAnalyzerForIndexFields(locale: Locale) extends JochreAnalyzerBase(locale) {
  private val log = LoggerFactory.getLogger(getClass)

  val indexingHelper = IndexingHelper()

  override def finalFilter(tokens: TokenStream): TokenStream = (textNormalizingFilter(_))
    .andThen(regexTokenizerFilter)
    .andThen(lowercaseFilter)
    .andThen(skipPunctuationFilter)
    .andThenIf(log.isTraceEnabled)(tapFilter(log, "final") _)
    .apply(tokens)
}
