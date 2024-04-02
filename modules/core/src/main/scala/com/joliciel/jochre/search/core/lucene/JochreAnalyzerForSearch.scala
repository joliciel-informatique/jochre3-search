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

class JochreAnalyzerForSearch(locale: Locale, forPhrases: Boolean) extends JochreAnalyzerBase(locale) {
  private val log = LoggerFactory.getLogger(getClass)
  private val config = ConfigFactory.load().getConfig("jochre.search")

  private val wordAnalyzer = new JochreAnalyzerForWords(locale)
  private val synonymMap = SynonymMapReader.getSynonymMap(locale, wordAnalyzer)

  override def finalFilter(tokens: TokenStream): TokenStream = (textNormalizingFilter(_))
    .andThen(regexTokenizerFilter)
    .andThenIf(forPhrases)(skipWildcardFilter)
    .andThenIf(!forPhrases)(stopWordFilter)
    .andThen(lowercaseFilter)
    .andThen(skipPunctuationFilter)
    .andThen(synonymFilter)
    .andThenIf(log.isTraceEnabled)(tapFilter(log, "final") _)
    .apply(tokens)

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
