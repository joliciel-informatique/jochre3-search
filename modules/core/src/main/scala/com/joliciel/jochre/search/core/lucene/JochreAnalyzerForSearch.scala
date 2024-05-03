package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.lucene.tokenizer.SkipWildcardInSearchFilter
import com.joliciel.jochre.search.core.text.SynonymMapReader
import com.joliciel.jochre.search.core.util.AndThenIf.Implicits._
import com.typesafe.config.ConfigFactory
import org.apache.lucene.analysis.core.StopFilter
import org.apache.lucene.analysis.synonym.SynonymGraphFilter
import org.apache.lucene.analysis.{CharArraySet, TokenStream}
import org.slf4j.LoggerFactory

import java.util.Locale

class JochreAnalyzerForSearch(
    locale: Locale,
    forPhrases: Boolean,
    addSynonyms: Boolean,
    languageSpecificFilters: Option[LanguageSpecificFilters] = None
) extends JochreAnalyzerBase(locale) {
  private val log = LoggerFactory.getLogger(getClass)
  private val config = ConfigFactory.load().getConfig("jochre.search")

  private val wordAnalyzer = new JochreAnalyzerForWords(locale)
  private val synonymMap = SynonymMapReader.getSynonymMap(locale, wordAnalyzer)

  private val maybePostTokenizationFilter = languageSpecificFilters.flatMap(_.postTokenizationFilter)
  private def postTokenizationFilter(tokens: TokenStream): TokenStream = {
    maybePostTokenizationFilter.map(_(tokens)).getOrElse(tokens)
  }

  override def finalFilter(tokens: TokenStream): TokenStream = {
    (textNormalizingFilter(_))
      .andThen(regexTokenizerFilter)
      .andThen(lowercaseFilter)
      .andThen(postTokenizationFilter)
      .andThenIf(log.isTraceEnabled)(tapFilter(log, "postTokenizationFilter") _)
      .andThenIf(!forPhrases)(stopWordFilter)
      .andThenIf(log.isTraceEnabled)(tapFilter(log, "stopWordFilter") _)
      .andThenIf(addSynonyms)(synonymFilter)
      .andThenIf(log.isTraceEnabled)(tapFilter(log, "synonymFilter") _)
      .andThenIf(forPhrases)(skipWildcardFilter)
      .andThenIf(log.isTraceEnabled)(tapFilter(log, "skipWildcardFilter") _)
      .andThen(skipPunctuationFilter)
      .andThenIf(log.isTraceEnabled)(tapFilter(log, "final") _)
      .apply(tokens)
  }

  private def skipWildcardFilter(tokens: TokenStream): TokenStream = new SkipWildcardInSearchFilter(tokens)

  protected def synonymFilter(tokens: TokenStream): TokenStream = {
    synonymMap
      .map { synonymMap =>
        new SynonymGraphFilter(tokens, synonymMap, true)
      }
      .getOrElse {
        tokens
      }
  }

  protected def stopWordFilter(tokens: TokenStream): TokenStream = {
    val key = f"${locale.getLanguage}.stop-words"
    val stopWords = Option.when(config.hasPath(key))(config.getStringList(key))
    stopWords
      .map { stopWords =>
        val stopSet = new CharArraySet(stopWords, false)
        new StopFilter(tokens, stopSet)
      }
      .getOrElse {
        tokens
      }
  }
}
