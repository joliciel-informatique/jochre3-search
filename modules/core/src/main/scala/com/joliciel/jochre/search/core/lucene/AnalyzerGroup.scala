package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.text.LanguageSpecificFilters
import com.typesafe.config.ConfigFactory
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.search.IndexSearcher

import java.util.Locale

private[search] case class AnalyzerGroup(
    forIndexing: JochreAnalyzerForIndex,
    forIndexingFields: Analyzer,
    forSearch: Analyzer,
    forSearchPhrases: Analyzer,
    forStrictSearch: Analyzer,
    forStrictSearchPhrases: Analyzer
)

private[search] object AnalyzerGroup {
  private val config = ConfigFactory.load().getConfig("jochre.search")
  private val locale: Locale = Locale.forLanguageTag(config.getString("locale"))
  private val maxClauseCount = config.getInt("lucene-query-max-clause-count")
  IndexSearcher.setMaxClauseCount(maxClauseCount)

  def generic(languageSpecificFilters: Option[LanguageSpecificFilters]): AnalyzerGroup =
    AnalyzerGroup(
      forIndexing = new JochreAnalyzerForIndex(locale, languageSpecificFilters),
      forIndexingFields = new JochreAnalyzerForIndexFields(locale, languageSpecificFilters),
      forSearch = new JochreAnalyzerForSearch(locale, forPhrases = false, addSynonyms = true, languageSpecificFilters),
      forSearchPhrases =
        new JochreAnalyzerForSearch(locale, forPhrases = true, addSynonyms = true, languageSpecificFilters),
      forStrictSearch =
        new JochreAnalyzerForSearch(locale, forPhrases = false, addSynonyms = false, languageSpecificFilters),
      forStrictSearchPhrases =
        new JochreAnalyzerForSearch(locale, forPhrases = true, addSynonyms = false, languageSpecificFilters)
    )
}
