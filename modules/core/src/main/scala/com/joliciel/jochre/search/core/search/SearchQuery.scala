package com.joliciel.jochre.search.core.search

case class SearchQuery(criterion: SearchCriterion)

sealed trait SearchCriterion

case class Contains(queryString: String) extends SearchCriterion
