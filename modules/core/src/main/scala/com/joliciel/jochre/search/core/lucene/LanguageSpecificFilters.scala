package com.joliciel.jochre.search.core.lucene

import org.apache.lucene.analysis.TokenStream

trait LanguageSpecificFilters {
  def postTokenizationFilter: Option[TokenStream => TokenStream]
}
