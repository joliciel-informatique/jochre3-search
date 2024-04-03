package com.joliciel.jochre.search.core.lucene

import com.typesafe.config.ConfigFactory
import org.apache.lucene.analysis.Analyzer

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

  val generic: AnalyzerGroup =
    AnalyzerGroup(
      forIndexing = new JochreAnalyzerForIndex(locale),
      forIndexingFields = new JochreAnalyzerForIndexFields(locale),
      forSearch = new JochreAnalyzerForSearch(locale, forPhrases = false, addSynonyms = true),
      forSearchPhrases = new JochreAnalyzerForSearch(locale, forPhrases = true, addSynonyms = true),
      forStrictSearch = new JochreAnalyzerForSearch(locale, forPhrases = false, addSynonyms = false),
      forStrictSearchPhrases = new JochreAnalyzerForSearch(locale, forPhrases = true, addSynonyms = false)
    )
}
