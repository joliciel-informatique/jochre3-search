package com.joliciel.jochre.search.core.lucene

import org.apache.lucene.analysis.TokenStream

import java.util.Locale

class JochreAnalyzerForWords(locale: Locale) extends JochreAnalyzerBase(locale) {

  override def finalFilter(tokens: TokenStream): TokenStream = (textNormalizingFilter(_))
    .andThen(regexTokenizerFilter)
    .andThen(lowercaseFilter)
    .andThen(skipPunctuationFilter)
    .apply(tokens)
}
