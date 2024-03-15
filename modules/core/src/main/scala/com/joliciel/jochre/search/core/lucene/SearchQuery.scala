package com.joliciel.jochre.search.core.lucene

import org.apache.lucene.search.Query

case class SearchQuery(criterion: SearchCriterion) {
  def toLuceneQuery(jochreSearcher: JochreSearcher): Query = criterion match {
    case Contains(queryString) =>
      val parser = new JochreMultiFieldQueryParser(
        Seq(LuceneField.Text),
        jochreSearcher.analyzerGroup.forSearch,
        jochreSearcher.analyzerGroup.forSearchPhrases
      )
      parser.parse(queryString)
  }
}

sealed trait SearchCriterion

case class Contains(queryString: String) extends SearchCriterion
