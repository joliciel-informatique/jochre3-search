package com.joliciel.jochre.search.core.lucene

import com.typesafe.config.ConfigFactory
import org.apache.lucene.analysis.Analyzer

import java.util.Locale

private[search] case class AnalyzerGroup(
    forIndexing: JochreAnalyzerForIndex,
    forSearch: Analyzer,
    forSearchPhrases: Analyzer
)

private[search] object AnalyzerGroup {
  private val config = ConfigFactory.load().getConfig("jochre.search")
  private val locale: Locale = Locale.forLanguageTag(config.getString("locale"))
  val generic: AnalyzerGroup =
    AnalyzerGroup(
      forIndexing = new JochreAnalyzerForIndex(locale),
      forSearch = new JochreAnalyzerForSearch(locale, forPhrases = false),
      forSearchPhrases = new JochreAnalyzerForSearch(locale, forPhrases = true)
    )
}
