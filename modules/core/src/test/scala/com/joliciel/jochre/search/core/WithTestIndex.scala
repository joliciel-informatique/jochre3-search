package com.joliciel.jochre.search.core

import com.joliciel.jochre.search.core.lucene.{AnalyzerGroup, JochreIndex}
import org.apache.lucene.store.MMapDirectory

import java.io.File

trait WithTestIndex {
  val analyzerGroup: AnalyzerGroup = AnalyzerGroup.generic(languageSpecificFilters = None)

  def getJochreIndex: JochreIndex = {
    val tempDir = File.createTempFile("jochre-test-index", ".tmp")
    tempDir.delete()
    tempDir.deleteOnExit()
    val indexDir = new MMapDirectory(tempDir.toPath)
    val jochreIndex = JochreIndex(indexDir, analyzerGroup)
    jochreIndex.indexer.commit()
    jochreIndex
  }
}
