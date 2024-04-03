package com.joliciel.jochre.search.core

case class SearchQuery(criterion: SearchCriterion)

sealed trait SearchCriterion

case class Contains(queryString: String) extends SearchCriterion

case class AuthorStartsWith(prefix: String) extends SearchCriterion
