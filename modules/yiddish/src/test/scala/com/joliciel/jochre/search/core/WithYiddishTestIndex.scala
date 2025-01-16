package com.joliciel.jochre.search.core

import com.joliciel.jochre.search.core.lucene.{AnalyzerGroup, JochreIndex}
import org.apache.lucene.store.MMapDirectory

import java.io.File
import com.joliciel.jochre.search.yiddish.YiddishFilters

trait WithYiddishTestIndex extends WithTestIndex {
  override val analyzerGroup: AnalyzerGroup = AnalyzerGroup.generic(languageSpecificFilters = Some(YiddishFilters))
}
