package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.lucene.tokenizer.SkipWildcardInSearchFilter
import com.joliciel.jochre.search.core.text.SynonymMapReader
import com.joliciel.jochre.search.core.util.AndThenIf.Implicits._
import org.apache.lucene.analysis.TokenStream
import org.apache.lucene.analysis.synonym.SynonymGraphFilter
import org.slf4j.LoggerFactory

import java.util.Locale

class JochreAnalyzerForSearch(locale: Locale, forPhrases: Boolean) extends JochreAnalyzerBase(locale) {
  private val log = LoggerFactory.getLogger(getClass)

  val wordAnalyzer = new JochreAnalyzerForWords(locale)

  override def finalFilter(tokens: TokenStream): TokenStream = (textNormalizingFilter(_))
    .andThen(regexTokenizerFilter)
    .andThenIf(forPhrases)(skipWildcardFilter _)
    .andThen(lowercaseFilter)
    .andThen(skipPunctuationFilter)
    .andThen(synonymFilter)
    .andThenIf(log.isTraceEnabled)(tapFilter(log, "final") _)
    .apply(tokens)

  private def skipWildcardFilter(tokens: TokenStream): TokenStream = new SkipWildcardInSearchFilter(tokens)

  protected def synonymFilter(tokens: TokenStream): TokenStream = {
    val synonymMap = SynonymMapReader.getSynonymMap(locale, wordAnalyzer)
    synonymMap
      .map { synonymMap =>
        new SynonymGraphFilter(tokens, synonymMap, true)
      }
      .getOrElse {
        tokens
      }
  }
}
