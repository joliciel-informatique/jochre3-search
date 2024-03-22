package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.util.AndThenIf.Implicits._
import org.apache.lucene.analysis.TokenStream
import org.slf4j.LoggerFactory

import java.util.Locale

class JochreAnalyzerForSearch(locale: Locale) extends JochreAnalyzerBase(locale) {
  private val log = LoggerFactory.getLogger(getClass)

  override def finalFilter(tokens: TokenStream): TokenStream = (regexTokenizerFilter(_))
    .andThenIf(log.isTraceEnabled)(tapFilter(log, "final") _)
    .apply(tokens)
}
