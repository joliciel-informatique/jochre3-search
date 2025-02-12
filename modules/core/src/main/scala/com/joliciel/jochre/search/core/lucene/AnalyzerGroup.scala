package com.joliciel.jochre.search.core.lucene

import com.joliciel.jochre.search.core.text.LanguageSpecificFilters
import com.typesafe.config.ConfigFactory
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.search.IndexSearcher

import java.util.Locale

import scala.jdk.CollectionConverters._
import org.slf4j.LoggerFactory
import com.joliciel.jochre.search.core.SearchQuery
import com.joliciel.jochre.search.core.SearchCriterion

private[search] case class AnalyzerGroup(
    forIndexing: JochreAnalyzerForIndex,
    forIndexingFields: Analyzer,
    forIndexingUntokenizedFields: Analyzer,
    forSearch: Analyzer,
    forSearchPhrases: Analyzer,
    forStrictSearch: Analyzer,
    forStrictSearchPhrases: Analyzer,
    languageSpecificFilters: Option[LanguageSpecificFilters]
) {
  private val log = LoggerFactory.getLogger(getClass)

  private val config = ConfigFactory.load().getConfig("jochre.search")
  private val queryFindReplacePairs = config
    .getConfigList("query-replacements")
    .asScala
    .map(c => c.getString("find").r -> c.getString("replace"))
    .map { case (find, replace) =>
      log.info(f"Added query replacement: FIND $find REPLACE $replace")
      find -> replace
    }
    .toSeq ++ languageSpecificFilters.map(_.queryFindReplacePairs).getOrElse(Seq.empty)

  def preformatQuery(query: SearchQuery): SearchQuery = {
    queryFindReplacePairs.foldLeft(query) { case (query, (find, replace)) =>
      query.replaceQuery((s: String) => find.replaceAllIn(s, replace))
    }
  }

  def preformatCriterion(criterion: SearchCriterion): SearchCriterion = {
    queryFindReplacePairs.foldLeft(criterion) { case (criterion, (find, replace)) =>
      criterion.replaceCriterion((s: String) => find.replaceAllIn(s, replace))
    }
  }
}

private[search] object AnalyzerGroup {
  private val config = ConfigFactory.load().getConfig("jochre.search")
  private val locale: Locale = Locale.forLanguageTag(config.getString("locale"))
  private val maxClauseCount = config.getInt("lucene-query-max-clause-count")
  IndexSearcher.setMaxClauseCount(maxClauseCount)

  def generic(languageSpecificFilters: Option[LanguageSpecificFilters]): AnalyzerGroup =
    AnalyzerGroup(
      forIndexing = new JochreAnalyzerForIndex(locale, languageSpecificFilters),
      forIndexingFields = new JochreAnalyzerForIndexFields(locale, tokenize = true, languageSpecificFilters),
      forIndexingUntokenizedFields =
        new JochreAnalyzerForIndexFields(locale, tokenize = false, languageSpecificFilters),
      forSearch = new JochreAnalyzerForSearch(locale, forPhrases = false, addSynonyms = true, languageSpecificFilters),
      forSearchPhrases =
        new JochreAnalyzerForSearch(locale, forPhrases = true, addSynonyms = true, languageSpecificFilters),
      forStrictSearch =
        new JochreAnalyzerForSearch(locale, forPhrases = false, addSynonyms = false, languageSpecificFilters),
      forStrictSearchPhrases =
        new JochreAnalyzerForSearch(locale, forPhrases = true, addSynonyms = false, languageSpecificFilters),
      languageSpecificFilters
    )
}
